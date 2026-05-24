# SEH (Structured Exception Handling) 设计方案

## 一、概述

本文档描述 evalit 脚本语言虚拟机的结构化异常处理（SEH）设计方案。目标是提供高效、可靠的 `try-catch-throw` 异常处理机制，支持异常回溯展开（unwinding），并最小化运行时开销。

### 1.1 设计目标

| 目标 | 说明 |
|------|------|
| 正确性 | 异常发生时正确回溯到 handler，恢复 VM 状态 |
| 效率 | 正常路径（无异常）零额外开销 |
| 嵌套支持 | 支持 try-catch 嵌套、函数调用嵌套 |
| 寄存器安全 | throw 时保护所有活跃寄存器，catch 时正确恢复 |
| 最小侵入 | 编译器、寄存器分配器改动最小 |

### 1.2 当前实现状态

当前代码库已有完整的 SEH 基础实现，所有 P0 正确性问题已修复：

| 组件 | 状态 | 文件 |
|------|------|------|
| 语法解析 (try/catch/throw) | ✅ 已完成 | `grammar.pest`, `syntax.rs`, `parser.rs` |
| AST 节点 | ✅ 已完成 | `syntax.rs` |
| IR 指令 (PushSeh/PopSeh/Throw/LoadException) | ✅ 已完成 | `instruction.rs` |
| IR Builder 方法 | ✅ 已完成 | `builder.rs` |
| 降级 (lowering: try/throw → IR) | ✅ 已完成 | `lowering.rs` |
| SSA 转换（异常边 + Phi 参数传递） | ✅ 已完成 | `ssabuilder.rs` |
| 代码生成 (IR → 字节码，Phi 参数 → Mov) | ✅ 已完成 | `codegen.rs` |
| 字节码 Opcode (Try/EndTry/ThrowExc/LoadException) | ✅ 已完成 | `bytecode.rs` |
| VM 执行（SEH 栈 + 跨帧展开） | ✅ 已完成 | `vm.rs` |
| 测试用例（6 个场景） | ✅ 全部通过 | `test_seh.rs` |

---

## 二、异常模型

### 2.1 异常值

脚本语言中的异常是**一等公民**（first-class value），可以是任何类型：

```rust
throw 42;               // 整数异常
throw "error msg";      // 字符串异常
throw { code: 404 };    // 对象异常
```

VM 中异常值以 `ValueRef` 表示，通过寄存器 `Rv` 在 throw/catch 之间传递。

### 2.2 语法

```
try_statement = { "try" ~ block ~ "catch" ~ pattern ~ block }
throw_statement = { "throw" ~ expression ~ ";" }
```

- `try` 块标记受保护的代码区域
- `catch` 块接收异常值，通过模式匹配（支持标识符绑定和通配符 `_`）
- `throw` 语句抛出异常值

### 2.3 语义规则

1. 若 `try` 块正常执行完毕（未抛出异常），跳过 `catch` 块
2. 若 `try` 块中抛出异常，控制立即转移到匹配的 `catch` 块
3. `catch` 块执行完毕后，继续执行 `try-catch` 之后的代码
4. 若 `try` 块中的异常未被内层处理，向外层 `try-catch` 传播
5. 若异常传播到函数边界，继续向调用方传播（跨帧展开）
6. 若异常传播到顶层（无任何 handler），VM 返回 `RuntimeError::UnhandledException`

---

## 三、SEH 栈设计

### 3.1 SehRecord 结构（当前实现）

```rust
#[derive(Debug, Clone)]
pub struct SehRecord {
    /// catch handler 的指令地址 (绝对地址)
    pub handler_pc: usize,
    /// 保存 try 入口处的数据栈指针 (rsp)
    pub saved_rsp: usize,
    /// 保存 try 入口处的控制栈指针 (ctrl_rsp)
    pub saved_ctrl_rsp: usize,
    /// 保存 try 入口处的基址指针 (rbp)
    pub saved_rbp: usize,
    // 待优化字段（见阶段 3/4）：
    // pub register_bitmap: u16,    // try 入口处的活跃寄存器位图
    // pub caller_link: Option<usize>,  // 调用方 SEH 链接
}
```

### 3.2 SEH 栈操作

```
try {
    // PushSeh(handler_offset)
    //   → 计算 handler 绝对地址
    //   → 保存当前 rsp/ctrl_rsp/rbp
    //   → 压入 seh_stack
    ...
    // EndTry
    //   → 弹出 seh_stack
} catch e {
    // handler_pc
    ...
}
```

**关键行为**：
- `Try` 指令：在 `seh_stack` 上压入一个新的 `SehRecord`，记录当前 VM 状态快照
- `EndTry` 指令：从 `seh_stack` 弹出对应的 `SehRecord`（正常路径清理）
- `Throw` 指令：从 `seh_stack` 弹出 `SehRecord`，恢复状态，跳转到 handler

### 3.3 嵌套 SEH 示意

```
try {                          // ← PushSeh A (seh_stack = [A])
    try {                      // ← PushSeh B (seh_stack = [A, B])
        throw 42;              // ← 弹出 B，恢复状态，跳转到 B.handler
    } catch e {                // ← B.handler 处理
        // 处理完毕，正常流程
    }                          // ← EndTry B (若未抛出)
} catch e {                    // ← A.handler (若 B 未捕获)
}
// EndTry A (若未抛出)
```

---

## 四、跨函数调用的异常展开 (Unwinding)

### 4.1 问题

当前实现仅支持**单帧内**的 SEH。若异常在函数调用深层抛出，需要穿过中间帧回退到包含 handler 的帧。

```
function inner() {
    throw "error";  // 无 handler
}
function outer() {
    try {
        inner();    // 异常从这里逃逸
    } catch e {
        // 需要在这里捕获
    }
}
```

### 4.2 跨帧展开方案

**核心思想**：函数调用时，将调用方的 SEH 栈深度记录在控制栈上。异常展开时：
1. 从当前 `seh_stack` 弹出记录，恢复 rsp/ctrl_rsp/rbp
2. 若恢复后的 rbp 与当前帧的 rbp 不同，说明跨了函数边界
3. 继续从恢复后的 `seh_stack` 弹出记录，直到找到 handler 或回到调用方

**控制栈增强**：

```
Call function:
    pushc(seh_stack.len())   // 记录当前 SEH 栈深度
    pushc(rbp)               // 保存当前 rbp
    pushc(return_pc)         // 保存返回地址
    // 进入函数

Ret from function:
    popc(return_pc)
    popc(saved_rbp)
    popc(saved_seh_depth)
    // 截断 seh_stack 到 saved_seh_depth（清理函数内注册的 handler）
```

**Throw 时的展开逻辑**：

```
handle_throw(exc_val):
    // 弹出 SEH 记录
    record = seh_stack.pop()
    if record is None:
        return UnhandledException

    // 恢复栈指针和执行环境
    rsp = record.saved_rsp
    ctrl_rsp = record.saved_ctrl_rsp
    rbp = record.saved_rbp

    // 重要：检查是否跨函数边界
    // 若 handler 属于不同函数（通过 rbp 判断），
    // 需要从调用方的控制栈恢复执行环境
    //
    // 实际上，我们通过恢复 rsp/rbp 到 try 入口点，
    // 然后 handler_pc 指向的函数内 catch 块，
    // 自动实现了跨帧展开的效果

    // 保存当前寄存器到数据栈（在恢复后的 rsp 位置）
    for each GP register:
        push(saved_reg_value)

    // 设置异常值到 Rv
    Rv = exc_val

    // 跳转到 handler
    jump(record.handler_pc)
```

### 4.3 控制栈的调用帧布局

```
控制栈 (ctrl_stack) 布局 (从低地址到高地址):

函数 A 调用函数 B:
┌─────────────────────────────┐
│ A 的返回地址                 │ ← ctrl_rsp 增长方向
│ A 的 rbp                     │
│ A 调用 B 时的 seh_stack 深度  │
│ B 的返回地址                 │
│ B 的 rbp                     │
│ B 入口时的 seh_stack 深度     │
│ ...                          │
└─────────────────────────────┘
```

**Ret 指令的展开流程**：
```
Ret:
    let return_pc = popc()
    let saved_rbp = popc()
    let saved_seh_depth = popc()
    rbp = saved_rbp
    truncate seh_stack to saved_seh_depth  // 关键：清理函数内注册的 SEH
    jump(return_pc)
```

---

## 五、寄存器管理

### 5.1 当前问题

当前实现中，`handle_throw` **总是保存所有 16 个 GP 寄存器**，这是最大的性能问题：

```rust
// 当前实现：低效
let saved_regs: Vec<ValueRef> = Register::general()
    .iter()
    .map(|reg| self.state.get_register(*reg).unwrap_or(ValueRef::null()))
    .collect();
```

这种方式的问题：
1. 保存了根本不在使用的寄存器（浪费内存和拷贝时间）
2. `ValueRef::clone()` 涉及 Rc/Arc 的引用计数操作
3. `LoadException` 无条件弹出所有寄存器，即使 catch 块只使用部分

### 5.2 优化方案：仅保存活跃寄存器

**编译器驱动**：在编译 try 块时，分析 try 块内**定义且被 catch 块使用**的变量（交叉活跃变量），仅保存这些变量对应的寄存器。

```
// 编译时分析：
// try 块内定义且 catch 块使用的变量 = {v1, v3, v7}
// 映射到物理寄存器 = {R0, R2, R5}

// 生成的指令：
try:
    // 隐式：保存 R0, R2, R5 (由编译器插入 Push 指令)
    // ... try 块代码 ...
    // 隐式：恢复 R0, R2, R5 (由编译器插入 Pop 指令)
    jump after_catch

handler:
    // 异常值在 Rv 中
    // 已恢复 R0, R2, R5
    // ... catch 块代码 ...
```

**但这个方案的问题是**：异常可能在 try 块中的任何位置发生，无法在静态编译时确定哪些寄存器"当前活跃"。更稳妥的做法是采用**编译驱动的完整寄存器位图**。

### 5.3 最终方案：寄存器位图 + 编译时标记

```
方案：使用位图标记 try 块入口处的所有"脏寄存器"

在 Try 指令中嵌入寄存器位图：
  Try(handler_offset, register_bitmap)
  
  register_bitmap: u16 位图
  - bit i = 1 表示 Ri 在 try 入口处有活跃值
  - throw 时仅保存位图标记的寄存器

编译器在 Try 指令生成时：
  1. 查询寄存器分配器，获取当前所有已分配的通用寄存器
  2. 生成位图作为 Try 指令的操作数
  
VM 在 throw 时：
  1. 从 Try 指令读取寄存器位图
  2. 仅保存位图标记的寄存器到数据栈
```

### 5.4 寄存器预留

为了支持 SEH 本身的操作（如保存/恢复寄存器、传递异常值等），需要预留部分寄存器：

| 寄存器 | 角色 | 说明 |
|--------|------|------|
| Rv | 异常值传递 | throw 时将异常值放入 Rv，catch 时从此读取 |
| Rsp | 栈指针 | SEH 恢复栈状态 |
| Rbp | 基址指针 | 帧标识，用于跨帧展开 |
| R0-R3 | 临时寄存器 | SEH 内部操作使用（预留，编译器不分配） |

**预留策略**：
- **Rv**：已预留，编译器不分配
- **Rsp/Rbp**：已预留，显式禁止编译器访问
- **R0-R3**：建议预留为临时寄存器（当前寄存器分配器中 `MIN_REQUIRED_REGISTER = 3` 已预留）

当前 16 个 GP 寄存器中，实际可分配给变量的约为 **R4-R15**（12 个），加上 R0-R3 中的部分（取决于具体分配策略）。

---

## 六、优化方案：寄存器分配的 SEH 感知

### 6.1 问题

当前的寄存器分配器（线性扫描）不考虑 SEH 路径。在 try-catch 场景中：

```
try_body:
    v1 = ...        // R0
    v2 = ...        // R1
    throw v1        // R0
                    // 问题：catch 需要 v2 的值(R1)，但 throw 已破坏栈

catch:
    exc = load_exception  // 恢复 R0,R1 等
    use(v2)               // v2 需要是 throw 之前的旧值
```

### 6.2 方案：活跃区间扩展

对于 try 块内定义、catch 块使用的变量，将它们的**活跃区间从正常出口扩展到虚拟出口（异常出口）**：

1. 在 CFG 中添加从 try_body 到 catch 块的异常边（当前已通过 `add_exception_edge` 支持）
2. 活跃性分析时，将 catch 块的 live_in 回溯到 try_body 的所有指令
3. 寄存器分配器在分配时会考虑这些扩展的活跃区间

### 6.3 CFG 中的异常边

```
         ┌─────────────┐
         │  try_entry   │
         │  PushSeh     │
         │  jump(try)   │
         └──────┬───────┘
                │
         ┌──────▼───────┐        异常边 (exception edge)
         │   try_body   │ ──────────────────────┐
         │   ...        │                       │
         │   PopSeh     │                       │
         │   jump(after)│                       │
         └──────┬───────┘                       │
                │                               │
         ┌──────▼───────┐              ┌────────▼────────┐
         │  after_catch  │              │   catch_blk     │
         │               │              │  load_exception │
         └───────────────┘              │  ...            │
                                        │  jump(after)    │
                                        └─────────────────┘
```

当前实现已添加异常边（`add_exception_edge`），但活跃性分析未处理这条边。任务计划中包含修复此问题。

### 6.4 寄存器分配策略优化

对于 try-catch 结构：

1. **优先分配寄存器**：跨越 try-catch 边界的变量优先获得物理寄存器（而非栈溢出）
2. **避免跨边界的溢出重载**：若一个变量在 try 和 catch 中都活跃，避免将其栈槽位复用于其他变量
3. **try 块内产生的临时值**：若只被 try 块使用，不受影响

---

## 七、字节码指令集扩展

### 7.1 现有指令

| 指令 | 操作数 | 说明 |
|------|--------|------|
| `Try` | `handler_offset: immd` | 注册 SEH handler |
| `EndTry` | 无 | 注销 SEH handler |
| `ThrowExc` | `src: reg` | 抛出异常 |
| `LoadException` | `dst: reg` | 加载捕获的异常值 |

### 7.2 指令优化

**Try 指令增强**：

```
Try(handler_offset: immd, register_bitmap: immd)
```

- `handler_offset`: 从当前指令到 handler 的相对偏移
- `register_bitmap`: 16 位寄存器位图（当前活跃的 GP 寄存器）

**ThrowExc 行为变更**：

```
ThrowExc src:
  1. 弹出 seh_stack 顶部记录
  2. 读取对应的 Try 指令的 register_bitmap
  3. 按位图保存活跃寄存器到数据栈
  4. 恢复 rsp/ctrl_rsp/rbp
  5. 设置 Rv = src
  6. jump(handler_pc)
```

### 7.3 新指令（可选）

| 指令 | 操作数 | 说明 |
|------|--------|------|
| `SaveRegs` | `bitmap: immd` | 按位图保存寄存器到栈（用于 try 入口） |
| `RestoreRegs` | `bitmap: immd` | 从栈恢复寄存器（用于 catch 入口） |

这些指令可以拆分 Try 的职责，但会引入更多指令。权衡后倾向于将位图编码到 Try 指令中。

---

## 八、VM State 变更

### 8.1 SehRecord 扩展（待实现）

```rust
// 计划扩展字段（尚未实现）：
pub struct SehRecord {
    pub handler_pc: usize,
    pub saved_rsp: usize,
    pub saved_ctrl_rsp: usize,
    pub saved_rbp: usize,
    pub register_bitmap: u16,   // 待添加：try 入口处的活跃寄存器位图
    pub caller_link: Option<usize>,  // 待添加：调用方 SEH 链接
}
```

### 8.2 控制栈调用帧变更

```rust
// 函数调用时压入控制栈：
pushc(seh_stack.len());   // 当前 SEH 深度
pushc(rbp);               // 当前基址指针
pushc(return_pc);         // 返回地址

// 函数返回时弹出：
let return_pc = popc();
let saved_rbp = popc();
let saved_seh_depth = popc();
seh_stack.truncate(saved_seh_depth);  // 清理函数内注册的 SEH
rbp = saved_rbp;
pc = return_pc;
```

### 8.3 vm.rs 中 run() 方法的变更（已完成）

**Call 指令**：在调用函数前，将当前 `seh_stack.len()` 保存到控制栈：

```rust
// 实际实现：
self.state.pushc(self.state.seh_stack.len())?;
self.state.pushc(self.state.pc() + 1)?;
self.state.jump(*location);
```

**Ret 指令**：恢复 SEH 栈深度到调用前的状态（清除被调用函数注册的所有 handler）：

```rust
// 实际实现：
let return_pc = self.state.popc()?;
let saved_seh_depth = self.state.popc()?;
self.state.seh_stack.truncate(saved_seh_depth);
self.state.jump(return_pc);
```

**handle_throw 的当前实现**（简化版本，不移除寄存器保存/恢复）：

```rust
fn handle_throw(&mut self, exc_val: ValueRef) -> Result<(), RuntimeError> {
    match self.state.seh_stack.pop() {
        Some(record) => {
            // 保留 throw 点的寄存器状态（不额外保存/恢复）
            // 通过 SSA Phi 参数传递保证 catch 块能取到正确变量值
            // 恢复 rsp/ctrl_rsp/rbp 到 try 入口点
            self.state.rsp = record.saved_rsp;
            self.state.ctrl_rsp = record.saved_ctrl_rsp;
            self.state.rbp = record.saved_rbp;
            // 设置异常值到 Rv
            self.state.set_register(Register::Rv, exc_val)?;
            // 跳转到 handler
            self.state.jump(record.handler_pc);
            Ok(())
        }
        None => Err(RuntimeError::UnhandledException { .. })
    }
}
```

**LoadException 的当前实现**（简化版本，不移除寄存器恢复）：

```rust
Opcode::LoadException => {
    // Registers are preserved at throw-point state by handle_throw.
    // Just get the exception value from Rv.
    let exc_val = self.state.get_register(Register::Rv)?;
    self.set_value(operands[0], exc_val)?;
}
```

---

## 九、编译器改动

### 9.1 寄存器分配器改动

1. **异常边活跃性传播**：在 `LiveIntervalAnalyzer::scan` 中，处理异常边导致的活跃性扩展
2. **寄存器预留**：修改 `RegisterSet`，支持预留寄存器（R0-R3 标记为不可分配）
3. **位图生成**：在 Codegen 生成 `Try` 指令时，从寄存器分配器获取当前活跃寄存器位图

### 9.2 Codegen 改动（已完成）

1. `PushSeh` 生成 `Try(handler_offset)`，其中 `handler_offset` 通过闭包修补（patch）在 block layout 确定后填入
2. `Throw` 指令的 codegen 从 `throw_to_handlers` 获取异常 handler 列表，仅为 handler 块生成 Phi 参数对应的 `Mov` 指令（传入 phi 参数的寄存器/栈位置）
3. `PopSeh` 生成 `EndTry` 指令
4. `LoadException` 生成 `LoadException dst` 指令
5. 调用序列中已实现 SEH 深度保存/恢复（见第 8 章）

### 9.3 Lowering 改动（已完成）

当前 lowering 已经正确生成 IR 序列：

1. `lower_try_stmt` 生成：
   ```
   PushSeh(catch_blk)  → 注册 SEH handler
   add_exception_edge(try_body, catch_blk)  → CFG 中添加从 try_body 到 catch 的异常边
   Jump(try_body)      → 跳转到 try 体
   try_body:
     ...               → try 体代码
     PopSeh            → 正常路径注销 SEH
     Jump(after_catch) → 正常路径跳转到合并点
   catch_blk:
     LoadException(dst) → 加载异常值
     ...               → catch 体代码
     Jump(after_catch)
   after_catch:
     ...               → 合并点
   ```

2. `lower_throw_stmt` 生成：
   ```
   Throw(value)        → 抛出异常
   ```

### 9.4 SSA Builder 改动（已完成）

SSA builder 已实现异常边处理和正确的 Phi 节点插入：

1. **异常边添加**：`add_exception_edges_for_throws()` 方法在 `convert_to_ssa()` 开始时扫描所有指令，为每个 `Throw` 指令的 **SEH 作用域中所有 handler** 添加异常边（而非仅最内层）。遇到 `Throw` 后立即停止当前块的指令扫描（`Throw` 之后的 `PopSeh` 等指令是死代码）。

2. **支配关系重算**：添加异常边后立即重新计算支配关系（`self.dominators = self.cfg.dominators()`），确保支配边界（dominance frontier）正确包含异常边，从而触发 catch handler 块的正确 Phi 节点放置。

3. **Phi 参数生成**：`add_jump_arguments()` 仅在目标块是 `throw_to_handlers` 中记录的异常 handler 时，才为 `Throw` 指令添加 Phi 参数。正常跳转目标（死代码路径）不添加，避免覆盖异常值。

4. **throw_to_handlers 传递**：`SSABuilder` 维护 `throw_to_handlers: HashMap<BlockId, Vec<BlockId>>` 映射，记录每个 throw 块对应的异常 handler 列表，并通过 `into_throw_to_handlers()` 方法传递给 Codegen。

**关键修复**：嵌套 try-catch 场景中，内层 throw 的异常边同时连接到内层和外层 handler，确保所有 catch 块都能正确接收活跃变量的当前版本（通过 Phi 参数）。

---

## 十、测试策略

### 10.1 基本功能测试（已有）
- ✅ try-catch 基本流程
- ✅ try-catch 无异常
- ✅ 异常值传递
- ✅ throw 无 catch（测试 UnhandledException）
- ✅ try-catch 嵌套
- ✅ catch 通配符 (`_`)

### 10.2 需要新增的测试

| 测试 | 说明 |
|------|------|
| 跨函数异常展开 | 在深层函数中 throw，外层 catch |
| 多层嵌套 + 函数调用 | 多个 try 嵌套 + 函数调用组合 |
| 寄存器正确性 | throw 后 catch 中访问 try 块定义的变量 |
| 异常路径的寄存器分配 | 确认异常路径不会破坏正常寄存器分配 |
| finally 语义（如后续添加） | 异常/正常路径都执行的清理代码 |
| try 操作符 `?` | 语法已解析，功能未实现 |
| 性能基准测试 | 对比正常路径 vs 异常路径的开销 |

---

## 十一、当前代码问题清单

### P0 - 正确性问题（✅ 已全部修复）

| # | 问题 | 状态 | 说明 |
|---|------|------|------|
| 1 | **SehRecord 缺少 saved_rbp** | ✅ 已修复 | `SehRecord` 增加 `saved_rbp` 字段，`Try` 指令保存 rbp，`handle_throw` 恢复 rbp |
| 2 | **SSA 异常边未参与支配关系计算** | ✅ 已修复 | 添加异常边后重新计算支配关系，确保 catch handler 块正确插入 Phi 节点 |
| 3 | **Throw 指令未传递 Phi 参数** | ✅ 已修复 | `Instruction::Throw` 增加 `args` 字段，SSA 转换后通过异常边传递活跃变量的当前版本 |
| 4 | **函数调用时 SEH 栈未保护** | ✅ 已修复 | `Call` 指令保存 `seh_stack.len()`，`Ret` 指令恢复 `seh_stack.truncate()` |
| 5 | **内层 catch 修改的变量未传递到外层** | ✅ 已修复 | `add_exception_edges` 为所有 handler 添加边（而非仅最内层），配合 Phi 参数传递 |

### P1 - 效率问题

| # | 问题 | 状态 | 说明 |
|---|------|------|------|
| 6 | **无条件保留所有寄存器** | ⏳ 待优化 | 当前 `handle_throw` 保留 throw 点的寄存器状态，不额外保存/恢复。可通过按位图仅保存活跃寄存器进一步优化 |
| 7 | **ValueRef::clone 开销** | ⏳ 待优化 | 当前通过 SSA Phi 参数 + Mov 指令传递变量值，减少了寄存器保存/恢复次数，但仍有优化空间 |

### P2 - 健壮性问题

| # | 问题 | 状态 | 说明 |
|---|------|------|------|
| 8 | **R0-R3 未预留** | ⏳ 待优化 | 当前所有 GP 寄存器都可被分配，SEH 本身需要临时寄存器 |
| 9 | **异常边未参与寄存器活跃性分析** | ⏳ 待优化 | catch 块需要使用的变量可能被错误分配/溢出 |

---

## 十二、分阶段实施计划和完成情况

### 阶段 1：修复基础 SEH 正确性（✅ 已完成）

| 任务 | 状态 | 说明 |
|------|------|------|
| 1a. SehRecord 增加 saved_rbp，Try 指令保存 rbp | ✅ 已完成 | `vm.rs` 中 `SehRecord` 增加 `saved_rbp`，`Try` 指令保存，`handle_throw` 恢复 |
| 1b. SSA 异常边正确处理 | ✅ 已完成 | 异常边添加后重新计算支配关系，throw 指令传递 Phi 参数 |
| 1c. 嵌套 try-catch 变量传递 | ✅ 已完成 | 为所有 handler 添加异常边，配合 Phi 参数实现变量版本正确传递 |
| 1d. 函数调用 SEH 栈保护 | ✅ 已完成 | Call/Ret 保存和恢复 seh_stack 深度 |

### 阶段 2：跨帧展开（⏳ 待完善）

| 任务 | 状态 | 说明 |
|------|------|------|
| 2a. Call 指令保存 seh_stack 深度到控制栈 | ✅ 已完成 | `pushc(seh_stack.len())` + `pushc(return_pc)` |
| 2b. Ret 指令从控制栈恢复 seh_stack 深度 | ✅ 已完成 | `popc()` 恢复 seh_stack 深度并 `truncate` |
| 2c. handle_throw 支持跨帧展开 | ⏳ 待完善 | 当前单帧内展开正确，跨函数帧需要进一步验证和测试 |
| 2d. 跨函数异常展开测试 | ⏳ 待实现 | 需要新增：在深层函数中 throw，外层 catch |

### 阶段 3：寄存器管理优化（⏳ 待实现）

| 任务 | 状态 | 说明 |
|------|------|------|
| 3a. Try 指令编码 register_bitmap | ⏳ 待实现 | 当前 `Try` 仅含 `handler_offset` |
| 3b. Throw 仅保存位图标记的寄存器 | ⏳ 待实现 | 当前保留 throw 点所有寄存器状态 |
| 3c. 预留 R0-R3 为 SEH 临时寄存器 | ⏳ 待实现 | 当前所有 GP 寄存器可被分配 |

### 阶段 4：寄存器分配器优化（⏳ 待实现）

| 任务 | 状态 | 说明 |
|------|------|------|
| 4a. 异常边的活跃性传播 | ⏳ 待实现 | catch 块使用变量的活跃区间需扩展到异常边 |
| 4b. try-catch 活跃变量的优先级分配 | ⏳ 待实现 | 跨边界的变量优先分配物理寄存器 |
| 4c. 寄存器位图的自动生成 | ⏳ 待实现 | Codegen 根据寄存器分配结果自动生成位图 |

### 阶段 5：功能扩展（可选）

| 任务 | 状态 | 说明 |
|------|------|------|
| 5a. `finally` 支持 | ⏳ 待实现 | `try { } catch { } finally { }` |
| 5b. `try` 操作符 `?` 的完整实现 | ⏳ 待实现 | 语法已解析，功能未实现 |
| 5c. 异常类型过滤 | ⏳ 待实现 | `catch e if condition` |