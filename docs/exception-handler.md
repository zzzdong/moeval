# 异常处理支持 — 实现文档

**版本**: 1.0  
**参考**: [Exceptions in Cranelift and Wasmtime](https://cfallin.org/blog/2025/11/06/exceptions/)  
**设计思路**: CFG 异常边 + 静态异常表 + 运行时栈展开

---

## 核心理念

- **异常边（Exception Edges）** 是 CFG 中的一等公民，存储于 `Block.exception_edges` 字段，但 **不加入 petgraph**。handler 块不会出现在 RPO 布局中，codegen 遍历所有块时按顺序输出指令。
- **零开销正常路径**：不抛异常时，异常表不被访问，无性能开销。
- **跨函数传播**：`ctrl_stack` 扩展为 `CallFrame { return_pc, func_id }`，`vm_throw` 沿调用链逐帧查表展开。

---

## 一、数据结构

### 1.1 ExceptionEdge（cfg.rs）

```rust
pub struct ExceptionEdge {
    pub catch_all: bool,          // true = 捕获所有异常
    pub handler: BlockId,         // 处理器入口块
    pub try_end_block: BlockId,   // try 区域的结束边界（merge 块）
}
```

### 1.2 ExceptionTableEntry（bytecode.rs）

```rust
pub struct ExceptionTableEntry {
    pub try_start: usize,    // 指令索引（包含）
    pub try_end: usize,      // 指令索引（不包含）
    pub handler: usize,      // 处理器入口指令索引
    pub catch_all: bool,
}
```

### 1.3 CallFrame（vm.rs）

```rust
pub struct CallFrame {
    pub return_pc: usize,
    pub func_id: FunctionId,
}

pub struct State {
    // ... 原有字段 ...
    pub ctrl_stack: [usize; STACK_MAX],       // 返回地址栈（原有，不变）
    pub call_frames: [CallFrame; STACK_MAX],  // 新增：函数调用帧栈
    pub call_rsp: usize,
}
```

`ctrl_stack` 仍负责返回地址（`pushc`/`popc`），`call_frames` 仅跟踪函数 ID。两者同步 push，`Ret` 只 pop `ctrl_stack`。

---

## 二、编译管线

### 2.1 语法（grammar.pest）

```pest
try_statement   = { "try" ~ block ~ catch_clause+ }
catch_clause    = { "catch" ~ pattern ~ block }
throw_statement = { "throw" ~ expression ~ ";" }
```

### 2.2 Lowering（lowering.rs）

try-catch lowering 流程：

```
原始 AST:
  try { body } catch (_) { handler }

生成的 CFG:
  entry ──Jump──▶ try_body ──Jump──▶ merge ◀──Jump── handler
                     │                           │
                     │  exception_edges = [       │
                     │    ExceptionEdge {         │
                     │      catch_all: true,       │
                     │      handler: handler,      │
                     │      try_end_block: merge,  │
                     │    }                       │
                     │  ]                         │
                     ▼                            │
                  handler ──Jump──────────────────┘
```

关键细节：

1. **Jump 链接**：`lower_try_stmt` 从当前 `curr_block` 发出 `Jump try_body`，确保 entry 不会 fall-through 到 handler
2. **TryContext 传播**：使用 `TryContext` 机制，记录 entering try 前的 block 集合，lowering try body 后，将所有新增块（try body 内部子块）加上异常边
3. **handler 前置**：handler 块在 lowering 中排在 try body 之前创建，但输出顺序由 codegen 按块顺序决定
4. **Throw 后 seal**：`lower_throw_stmt` 在 `throw` 指令后调用 `seal_block`，防止后续死代码

### 2.3 Codegen（codegen.rs）

handler 块和 try body 块在同一根 `for block in block_layout.iter(&cfg)` 循环中生成，
因为 handler 块虽然在 petgraph 中不可达，但作为 CFG 中的普通块被 `loop_root_reverse_postorder_layout2` 输出。

异常表生成时使用 `try_end_block` 作为 `try_end`，确保异常范围精确到 merge 块：

```rust
for block in cfg.blocks() {
    if block.exception_edges.is_empty() { continue; }
    let try_start = self.block_map[&block.id()];
    for edge in &block.exception_edges {
        let handler = self.block_map[&edge.handler];
        let try_end = self.block_map[&edge.try_end_block];
        exception_table.push(ExceptionTableEntry {
            try_start, try_end, handler, catch_all: edge.catch_all,
        });
    }
}
```

异常表函数内偏移在 Compiler 层统一加 `func_offset` 做重定位。

---

## 三、运行时

### 3.1 Throw 指令执行

```
Opcode::Throw:
  exception = get_value(payload_reg).take()
  vm_throw(exception)
```

### 3.2 vm_throw（核心）

```
vm_throw(exception):
  loop:
    current_func = state.current_func_id()
    pc = state.pc

    if handler = find_handler(current_func, pc):
      state.jump(handler)
      state.set_register(Rv, exception)   // 异常值通过 Rv 传递
      return

    if state.call_rsp == 0:
      return RuntimeError::UnhandledException

    frame = state.pop_frame()
    state.popc()          // 同步弹出返回地址
    state.jump(frame.return_pc)  // 继续在调用者帧中查找
```

### 3.3 find_handler（二分 + 最小嵌套优先）

```
find_handler(func_id, pc):
  table = exception_tables[func_id]

  // 二分查找最后一个 try_start <= pc 的条目
  idx = binary_search(table, pc)

  // 线性反向扫描，选择 try_end 最小的（最内层）
  for i in (idx..0).rev():
    if pc in [entry.try_start, entry.try_end) && entry.catch_all:
      if entry.try_end < best.try_end:
        best = entry

  return best.handler
```

### 3.4 Call/Ret 配合

```
Call func:
  pushc(return_pc)    // ctrl_stack（返回地址）
  push_frame({ return_pc, func_id })  // call_frames（函数 ID）
  jump(func_entry)

Ret:
  if ctrl_stack_reached_bottom():
    return Rv
  pop_frame()        // 忽略返回值，仅保持栈同步
  pc = popc()
  jump(pc)
```

---

## 四、嵌套处理

嵌套 try-catch 的异常表条目会有重叠的 `[try_start, try_end)` 范围。
`find_handler` 通过选择 `try_end` 最小的条目来确保内层 handler 优先于外层 handler：

```
异常表（嵌套示例）：
  [1, 14) → handler=1  // 外层
  [8, 10) → handler=5  // 内层 ← 选中（try_end 最小）

pc=9 时查找：
  idx = binary_search([1, 8, ...]) = 1 (指向 [8, 10))
  反向扫描：entry=[8,10) best=5, entry=[1,14) 跳过(try_end更大)
  result = handler=5  ← 内层处理器
```

---

## 五、测试覆盖

10 个测试用例，全部通过：

| 测试 | 场景 | 验证点 |
|------|------|--------|
| `test_try_no_throw` | 正常执行，不抛异常 | 走 body 路径，不进 catch |
| `test_try_catch_basic` | 抛出 → 捕获 | catch 块正确执行 |
| `test_try_catch_normal_path` | try 体内运算后正常返回 | 正常路径正确 |
| `test_try_catch_with_local_vars` | 局部变量跨 try/catch | 变量绑定正确 |
| `test_throw_unhandled` | 未捕获 throw | RuntimeError::UnhandledException |
| `test_nested_try_catch` | 内层捕获 | 内层 handler 优先于外层 |
| `test_try_catch_in_function` | 函数内 try-catch | 函数内异常表重定位正确 |
| `test_try_catch_cross_function_unwind` | inner→middle→outer(catch) | 2 层跨函数传播 |
| `test_try_catch_deep_unwind` | level3→level2→level1(catch) | 3 层跨函数传播 |
| `test_try_catch_unwind_no_handler_in_middle` | 中间函数无处理 | 跳过中间帧，外层 catch |

---

## 六、与原始方案的关键差异

| 维度 | 原始文档 | 实际实现 |
|------|---------|---------|
| CFG 异常边 | 加入 petgraph | ❌ 不加入 petgraph，handler 块在 RPO 布局中自然包含 |
| try_end 计算 | 使用 layout 中下一个块 | ✅ 使用 `try_end_block`（merge 块），精确且支持嵌套 |
| 嵌套匹配 | 第一个匹配 | ✅ 选择 `try_end` 最小的（最内层优先） |
| ctrl_stack | 改为 CallFrame 数组 | ✅ 保留原 `ctrl_stack`，新增独立 `call_frames` |
| 函数异常表 | 全局 | ✅ 按 `FunctionId` 索引，每个函数独立，编译时重定位 |
| handler 块代码生成 | 需独立遍历 | ✅ 全量遍历 `block_layout` 即可覆盖（handler 块在 RPO 中） |
| `?` 语法糖 | 本文档提及 | ❌ `?` 是 `Result` 的语法糖，与异常处理独立，本文档不包含 |
