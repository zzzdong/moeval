# SEH 异常处理实施任务书

## 概述

本任务书列出在 evalit VM 中实现完整 SEH (Structured Exception Handling) 支持的具体任务、优先级、依赖关系和预计工作量。

**设计文档**: [seh-exception-handling.md](file:///d:/code/rust/evalit/docs/seh-exception-handling.md)

---

## 阶段 1：修复基础 SEH 正确性 (P0)

### 任务 1.1：SehRecord 增加 saved_rbp 字段

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - `SehRecord` 结构体增加 `saved_rbp: usize` 字段
  - `Opcode::Try` 处理中保存 `self.state.rbp` 到 `saved_rbp`
  - `handle_throw` 中恢复 `self.state.rbp = record.saved_rbp`
- **验证**: 所有 `test_seh.rs` 中的测试通过
- **依赖**: 无
- **难度**: ⭐

### 任务 1.2：handle_throw 正确恢复寄存器

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - 重构 `handle_throw`：保存寄存器的顺序与 `LoadException` 中恢复的顺序必须一致
  - 当前代码保存顺序：R0→R1→R2→R3→R4（正向），弹出顺序：reverse（反向）
  - 修正保存数据结构为 `Vec<(Register, ValueRef)>` 以记录每个保存值的对应寄存器
  - `LoadException` 从 `seh_saved_regs` 元数据正确恢复
- **验证**: 寄存器值在 throw/catch 后保持正确的测试
- **依赖**: 任务 1.1
- **难度**: ⭐⭐

### 任务 1.3：修正 PushSeh/PopSeh 在 SSA Builder 中的处理

- **文件**: [ssabuilder.rs](file:///d:/code/rust/evalit/src/compiler/ir/ssabuilder.rs)
- **改动**:
  - 检查 `PushSeh` 是否被正确处理为非 terminator
  - 确保 `PopSeh` 和 `Throw` 之间的控制流正确
  - 修复可能存在的 Phi 节点插入问题
- **验证**: 编译含 try-catch 的脚本不崩溃或生成错误 IR
- **依赖**: 任务 1.1
- **难度**: ⭐⭐⭐

### 任务 1.4：修正 codegen 中 Try 指令的 handler 偏移计算

- **文件**: [codegen.rs](file:///d:/code/rust/evalit/src/compiler/codegen.rs)
- **改动**:
  - 当前的 patch 机制已正确计算偏移，但验证跨块边界时的正确性
  - 确保 `PushSeh` → `Jump` → `PopSeh` 序列在最终字节码中的偏移正确
- **验证**: 通过 `test_try_catch_basic` 等测试
- **依赖**: 无
- **难度**: ⭐

---

## 阶段 2：跨帧展开 (P0)

### 任务 2.1：函数调用时保存 SEH 栈深度

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs), [codegen.rs](file:///d:/code/rust/evalit/src/compiler/codegen.rs)
- **改动**:
  - `Opcode::Call` 处理中：调用前 `pushc(self.state.seh_stack.len())`
  - 对应的 codegen：在 `call` 指令前插入 `PushC(Rsp_SehDepth)`— 不对，这是 VM 层变更
  - 实际只需在 VM 的 `Opcode::Call` handler 中直接操作控制栈
  - 控制栈布局变更：`[return_pc, saved_rbp, seh_depth | return_pc, ...]`
- **验证**: 函数内 throw 被函数外 catch 捕获的测试
- **依赖**: 阶段 1
- **难度**: ⭐⭐

### 任务 2.2：Ret 指令恢复 SEH 栈深度

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - `Opcode::Ret` 指令处理中：
    1. `let seh_depth = self.state.popc()` — 先弹出 SEH 深度
    2. `let saved_rbp = self.state.popc()` — 再弹出 rbp
    3. `let return_pc = self.state.popc()` — 最后弹出返回地址
    4. `self.state.seh_stack.truncate(seh_depth)` — 清理函数内 SEH
    5. `self.state.rbp = saved_rbp`
    6. `self.state.jump(return_pc)`
  - 注意：Ret 指令的流程与当前不同，需重新实现
- **验证**: 函数返回后，函数内注册的 handler 不会影响调用方
- **依赖**: 任务 2.1
- **难度**: ⭐⭐

### 任务 2.3：控制栈调用帧布局全面变更

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - 当前 `Call` 只 `pushc(return_pc)`
  - 改为：`pushc(seh_stack.len())`, `pushc(rbp)`, `pushc(return_pc)`
  - 当前 `Ret` 只 `popc()` 得到返回地址
  - 改为：`popc()`→返回地址, `popc()`→rbp, `popc()`→seh_depth
  - 更新 `ctrl_stack_reached_bottom()` 检测逻辑
- **验证**: 所有函数调用测试 + SEH 测试通过
- **依赖**: 任务 2.1, 2.2
- **难度**: ⭐⭐⭐

### 任务 2.4：handle_throw 跨帧展开

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - `handle_throw` 在弹出 SEH 记录后，检查是否跨帧：
    - 恢复 rsp/ctrl_rsp/rbp 后，比较当前 `pc` 所在的函数（通过 `rbp` 判断）
    - 若跨帧，需要从控制栈恢复调用方的执行环境
    - 实际上，恢复 rbp 后，执行环境自然回到调用方帧
  - 关键在于：若 SEH 记录被清空仍未找到 handler，才返回 UnhandledException
- **验证**: 多层函数调用 + 异常展开的全面测试
- **依赖**: 任务 2.3
- **难度**: ⭐⭐⭐

---

## 阶段 3：寄存器管理优化 (P1)

### 任务 3.1：预留 R0-R3 为 SEH 临时寄存器

- **文件**: [bytecode.rs](file:///d:/code/rust/evalit/src/bytecode.rs), [regalloc.rs](file:///d:/code/rust/evalit/src/compiler/regalloc.rs)
- **改动**:
  - `MIN_REQUIRED_REGISTER` 从 3 增加到 4（R0-R3 预留）
  - `RegisterSet::new()` 从 `Register::all()` 排除 R0-R3（或允许配置预留集）
  - `Register::general()` 返回 R4-R15（或拆分为 `allocatable()` 和 `reserved()`）
- **验证**: 寄存器分配器正常工作，R0-R3 永远不会被分配给变量
- **依赖**: 无
- **难度**: ⭐

### 任务 3.2：Try 指令编码寄存器位图

- **文件**: [bytecode.rs](file:///d:/code/rust/evalit/src/bytecode.rs), [codegen.rs](file:///d:/code/rust/evalit/src/compiler/codegen.rs), [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - `Bytecode::triple` 中 Try 指令使用 operands[1] 编码寄存器位图
  - Codegen 中 `PushSeh` → 计算当前活跃寄存器位图 → 编码到指令
  - `SehRecord` 增加 `register_bitmap: u16` 字段
  - VM 的 `Opcode::Try` 从中读取位图并保存到 SehRecord
- **验证**: 生成的字节码中包含正确的位图信息
- **依赖**: 任务 3.1
- **难度**: ⭐⭐

### 任务 3.3：Throw 按位图保存寄存器

- **文件**: [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs)
- **改动**:
  - `handle_throw` 中，遍历 `Register::general()`，仅保存位图中标记为 1 的寄存器
  - 按位图顺序将值压入数据栈，同时记录保存的寄存器顺序
  - `LoadException` 按逆序恢复寄存器
- **验证**: 寄存器保存/恢复正确，测试中比较 throw 前后寄存器状态
- **依赖**: 任务 3.2
- **难度**: ⭐⭐

---

## 阶段 4：寄存器分配器 SEH 感知 (P1)

### 任务 4.1：异常边的活跃性传播

- **文件**: [regalloc.rs](file:///d:/code/rust/evalit/src/compiler/regalloc.rs), [cfg.rs](file:///d:/code/rust/evalit/src/compiler/ir/cfg.rs)
- **改动**:
  - `ControlFlowGraph` 支持标记"异常边"（exception edge）
  - `LiveIntervalAnalyzer::scan` 在处理基本块时，将异常边视为与正常边相同的活跃传播路径
  - 具体：在 `compute_block_liveness` 中，catch 块作为 try_body 的"伪后继"，将 catch 的 live_in 回溯到 try_body 末尾
- **验证**: try 块内定义、catch 块使用的变量不会被错误地溢出到栈
- **依赖**: 阶段 3
- **难度**: ⭐⭐⭐⭐

### 任务 4.2：try-catch 跨边界变量的优先级分配

- **文件**: [regalloc.rs](file:///d:/code/rust/evalit/src/compiler/regalloc.rs)
- **改动**:
  - 在 `arrange` 阶段，识别跨 try-catch 边界的变量
  - 提升这些变量的分配优先级（优先分配物理寄存器）
  - 避免将这些变量溢出到栈（栈溢出在 throw 恢复后可能导致值丢失）
- **验证**: 复杂 try-catch 嵌套场景中变量值正确性
- **依赖**: 任务 4.1
- **难度**: ⭐⭐⭐

### 任务 4.3：寄存器位图的自动生成

- **文件**: [codegen.rs](file:///d:/code/rust/evalit/src/compiler/codegen.rs), [regalloc.rs](file:///d:/code/rust/evalit/src/compiler/regalloc.rs)
- **改动**:
  - `RegAlloc` 增加 `fn current_register_bitmap(&self) -> u16` 方法
  - 返回当前所有已分配变量的寄存器位图
  - Codegen 中 `PushSeh` → 调用此方法获取位图 → 编码到 Try 指令
- **验证**: 位图精确反映当前活跃寄存器状态
- **依赖**: 任务 4.1, 4.2
- **难度**: ⭐⭐

---

## 阶段 5：功能扩展 (P2)

### 任务 5.1：finally 支持

- **改动范围**: 语法解析 → AST → IR → 字节码 → VM
- **说明**: `try { } catch { } finally { }` 的完整实现
- **验证**: finally 在正常和异常路径都执行的测试
- **依赖**: 阶段 2, 3
- **难度**: ⭐⭐⭐⭐

### 任务 5.2：try 操作符 `?` 支持

- **改动范围**: Lowering, IR, VM
- **说明**: `value?` 表达式的完整实现，自动传播异常
- **验证**: `?` 操作符正确传播异常的测试
- **依赖**: 阶段 2
- **难度**: ⭐⭐⭐

### 任务 5.3：异常过滤 (catch guard)

- **改动范围**: 语法解析 → AST → IR → 字节码 → VM
- **说明**: `catch e if condition { }` 的条件捕获
- **验证**: 只有满足条件的异常才被捕获
- **依赖**: 阶段 1, 2
- **难度**: ⭐⭐⭐

---

## 任务依赖关系图

```
阶段 1 (基础正确性)
  ├── 1.1 SehRecord.saved_rbp
  ├── 1.2 handle_throw 寄存器恢复
  ├── 1.3 SSA Builder 修复
  └── 1.4 codegen 偏移计算
       │
       ▼
阶段 2 (跨帧展开)
  ├── 2.1 Call 保存 SEH 深度
  ├── 2.2 Ret 恢复 SEH 深度
  ├── 2.3 控制栈布局变更
  └── 2.4 跨帧 handle_throw
       │
       ├─────────────────────┐
       ▼                     ▼
阶段 3 (寄存器优化)    阶段 5 (功能扩展)
  ├── 3.1 预留寄存器       ├── 5.1 finally
  ├── 3.2 位图编码         ├── 5.2 ? 操作符
  ├── 3.3 按位图保存       └── 5.3 catch guard
       │
       ▼
阶段 4 (分配器感知)
  ├── 4.1 异常边活跃传播
  ├── 4.2 优先级分配
  └── 4.3 位图自动生成
```

---

## 涉及文件清单

| 文件 | 阶段 | 改动类型 |
|------|------|----------|
| [vm.rs](file:///d:/code/rust/evalit/src/runtime/vm.rs) | 1,2,3 | ⭐⭐⭐⭐⭐ 核心改动 |
| [bytecode.rs](file:///d:/code/rust/evalit/src/bytecode.rs) | 3 | ⭐ Try 指令签名 |
| [codegen.rs](file:///d:/code/rust/evalit/src/compiler/codegen.rs) | 1,3,4 | ⭐⭐⭐ 指令生成 |
| [regalloc.rs](file:///d:/code/rust/evalit/src/compiler/regalloc.rs) | 3,4 | ⭐⭐⭐⭐ 分配器逻辑 |
| [cfg.rs](file:///d:/code/rust/evalit/src/compiler/ir/cfg.rs) | 4 | ⭐ 异常边标记 |
| [instruction.rs](file:///d:/code/rust/evalit/src/compiler/ir/instruction.rs) | 1 | ⭐ 无改动(已就绪) |
| [ssabuilder.rs](file:///d:/code/rust/evalit/src/compiler/ir/ssabuilder.rs) | 1 | ⭐⭐ PushSeh 处理 |
| [lowering.rs](file:///d:/code/rust/evalit/src/compiler/lowering.rs) | 1 | ⭐ 无改动(已就绪) |
| [builder.rs](file:///d:/code/rust/evalit/src/compiler/ir/builder.rs) | 1 | ⭐ 无改动(已就绪) |
| [syntax.rs](file:///d:/code/rust/evalit/src/compiler/ast/syntax.rs) | 5 | ⭐ 语法节点扩展 |
| [grammar.pest](file:///d:/code/rust/evalit/src/compiler/ast/grammar.pest) | 5 | ⭐ 文法扩展 |
| [test_seh.rs](file:///d:/code/rust/evalit/tests/test_seh.rs) | 全部 | ⭐⭐⭐ 持续扩展测试 |

---

## 测试验证计划

### 单元测试 (test_seh.rs)

```
阶段 1 验证:
  ✅ test_try_catch_basic         - 基本 try-catch-throw 流程
  ✅ test_try_catch_no_throw      - try 块无异常
  ✅ test_try_catch_caught_value  - 异常值传递
  ✅ test_throw_no_catch          - 未捕获异常
  ✅ test_try_catch_nested        - 嵌套 try-catch
  ✅ test_try_catch_wildcard      - 通配符模式
  
新增:
  ❏ test_try_catch_registers     - 寄存器在 throw/catch 后正确
  ❏ test_try_catch_nested_func   - 跨函数异常展开
  ❏ test_try_catch_multi_nested  - 多层嵌套 + 函数调用
  ❏ test_try_catch_finally       - finally 语义
  ❏ test_try_operator            - ? 操作符
  ❏ test_try_catch_guard         - catch 条件过滤
```

### 回归测试

- 每次提交前运行 `cargo test --test test_seh`
- 完整的 CI 测试：`cargo test`
- 确保非 SEH 相关测试不受影响

---

## 验收标准

1. 所有现有测试（含 test_seh.rs）通过
2. 跨函数异常展开正确工作
3. 寄存器在异常路径中保持正确
4. 正常路径零额外开销（通过基准测试验证）
5. 编译器管线完整支持 try-catch-throw

---

*版本: 1.0*
*日期: 2026-05-24*