# My Emacs Config (Emacs 30+ Optimized)

> 本文档由 AI 生成。

这份配置专为 **Emacs 30.0+** 设计，充分利用了 Emacs 30 的原生特性（Tree-sitter, Native Compilation, IO 优化等），打造了一个极速、现代化且稳定的开发环境。

## 核心特性

- **极致性能**: 
  - 启用 Emacs 30 的 `process-adaptive-read-buffering = nil`，LSP 响应速度飞跃。
  - 启用 `pixel-scroll-precision-mode`，支持触摸板像素级丝滑滚动。
  - 移除冗余的 UI 渲染（精简 LSP Inlay Hints）。
- **全栈 Tree-sitter**:
  - 全面迁移至 `*-ts-mode` (Go, Python, JS, TS, TSX, JSON, CSS, HTML)。
  - 使用 `treesit-auto` 自动安装和管理语法文件。
  - 极速语法高亮，秒开大文件。
- **现代化格式化**:
  - 引入 **Apheleia** 替换老旧的 `format-all`。
  - 真正的**异步格式化**：保存文件时后台运行 Prettier/Goimports，界面零卡顿，光标位置完美保持。
- **经典与现代的融合**:
  - **补全**: 保留经典的 **Company**，配合 LSP 智能后端。
  - **交互**: 采用现代化的 **Vertico + Consult + Marginalia** 体系替换 Helm/Ivy。

## 适用场景

- **MacOS GUI** (推荐) / Linux
- **Go 开发**: `go-ts-mode` + `gopls` + Inlay Hints (参数名提示) + `goimports`
- **前端开发**: `js-ts-mode`/`tsx-ts-mode` + `ts-ls` + Prettier (强制 2 空格)
- **Python 开发**: `python-ts-mode` + `pylsp` + Black
- **通用编辑**: Markdown, Org-mode

## 安装要求

1. **Emacs 30.0+**: 必须使用支持 Tree-sitter 的 Emacs 版本。
   ```sh
   brew tap d12frosted/emacs-plus
   brew install emacs-plus@30 --with-native-comp --with-poll --with-modern-icon
   ```

2. **外部依赖**:
   - **Ripgrep**: `brew install ripgrep`
   - **LSP Servers**:
     - Go: `go install golang.org/x/tools/gopls@latest`
     - Frontend: `npm install -g typescript-language-server typescript`
     - Python: `pip install python-lsp-server[all] flake8 black`
   - **Formatters**:
     - Prettier: `npm install -g prettier`

## 开发环境详情

### Go
- **Mode**: `go-ts-mode` (原生 Tree-sitter)
- **LSP**: 自动启动 `gopls`。
- **特性**: 
  - 开启了 **Inlay Hints**，函数调用时会自动显示参数名。
  - 保存时自动运行 `goimports` (通过 Apheleia)。
  - 支持 `treesit-fold` 代码折叠。

### Frontend (React/JS/TS)
- **Mode**: `js-ts-mode`, `tsx-ts-mode`, `typescript-ts-mode`。
- **LSP**: 使用 `typescript-language-server` (ts-ls)。
- **优化**:
  - **无干扰模式**: 关闭了 JS/TS 的 Inlay Hints (类型推导提示)，避免视觉杂乱和性能损耗。
  - **强制缩进**: 无论项目配置如何，Emacs 侧强制使用 **2 空格缩进**。
- **格式化**: 保存时自动调用 **Prettier** (通过 Apheleia)，强制 `--tab-width 2`。

### Python
- **Mode**: `python-ts-mode`。
- **LSP**: 使用 `pylsp`，集成了 `flake8` 检查。
- **格式化**: 保存时自动调用 **Black**。

## 常用快捷键

- **LSP**:
  - `M-.`: 跳转定义 (xref/lsp-ui-peek)
  - `M-?`: 查找引用
  - `C-c l r`: 重命名 (Rename)
  - `C-c l a`: Code Actions
- **Apheleia**:
  - `<f8> q`: 手动格式化当前 buffer
- **Tree-sitter**:
  - `C-'`: 折叠/展开当前代码块
  - `C-:`: 展开全部
  - `C-;`: 折叠全部

## 常见问题 (FAQ)

### Emacs 相关

1. **Tree-sitter 报错 "grammar unavailable"?**
   - 第一次打开某种语言文件时，`treesit-auto` 会自动提示你安装语法文件。按 `y` 确认并等待编译完成即可。
   - 或者手动运行 `M-x treesit-install-language-grammar`。

2. **补全前端是什么？**
   - 依然是 **Company**。保持了经典的 `C-n` / `C-p` 选择习惯，极速响应 (0.1s)。

3. **LSP UI 文档弹窗太烦？**
   - 当前配置为**手动触发模式**：光标移动时不会自动弹出文档，避免视线遮挡。
   - 触发方式：**鼠标悬停** 或使用快捷键（如 `M-.` 预览定义）。
   - 弹窗位置：跟随光标 (`at-point`)。

### Org Mode 相关 (Legacy)

1. **禁止 `_` 转义为下标？**
   - 可以在文件头 `#+OPTIONS` 中设置 `^:nil` 来全局禁用。

2. **TODO 优先级**
   - 任务可以分优先级 `[#A], [#B], [#C]` 三种。使用 `<shift> + <up/down>` 进行切换。
   - `org-sort-entries` 可对任务按优先级排序。

3. **控制标题展开**
   - 使用 `#+STARTUP: showall` 或 `overview` / `content` 来控制打开文件时的折叠状态。

4. **自动生成目录**
   - 安装 `toc-org` 后，在标题后加上 `:TOC:` 标签保存即可。

## 推荐资源 (References)

- **配置参考**:
  - [purcell/emacs.d](https://github.com/purcell/emacs.d): 经典的 Emacs 配置。
  - [Doom Emacs](https://github.com/hlissner/doom-emacs): 现代化的 Evil 配置框架。
  - [System Crafters](https://github.com/SystemCrafters/crafted-emacs): 极好的 Emacs 教学和配置。

- **学习资料**:
  - [Mastering Emacs](https://www.masteringemacs.org/): 必读经典。
  - [Emacs Rocks](http://emacsrocks.com/): 短视频介绍插件技巧。
