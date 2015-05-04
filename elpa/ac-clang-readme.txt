* INTRODUCTION:
  This program fork from auto-complete-clang-async.el
  ac-clang provide code completion and arguments expand.
  This program consists of the client(elisp) and server(binary).
  The server is executable file, and a self-build is necessary.
  The server achieve code completion using libclang of LLVM.

* FEATURES:
  - Basic(same auto-complete-clang-async)
    Auto Completion source for clang.
    uses a "completion server" process to utilize libclang.
    supports C/C++/Objective-C mode.
    jump to declaration or definition. return from jumped location.
    jump is an on-the-fly that doesn't use the tag file.
    also provides flymake syntax checking.
    a few bugfix and refactoring.

  - Extension
    "completion server" process is 1 process per Emacs. (original version is per buffer)
    supports template method parameters expand.
    supports manual completion.
    supports libclang CXTranslationUnit Flags.
    supports libclang CXCodeComplete Flags.
    supports multibyte.
    supports debug logger buffer.
    more a few modified. (client & server)

  - Optional
    supports CMake.
    clang-server.exe and libclang.dll built with Microsoft Visual Studio 2013.
    supports x86_64 Machine Architecture + Windows Platform. (Visual Studio Predefined Macros)

* EASY INSTALLATION(Windows Only):
  - Visual C++ Redistributable Packages for Visual Studio 2013
    must be installed if don't have a Visual Studio 2013.
    [http://www.microsoft.com/download/details.aspx?id=40784]

  - Completion Server Program
    built with Microsoft Visual Studio 2013.
    [https://github.com/yaruopooner/ac-clang/releases]
    1. download clang-server.zip
    2. clang-server.exe and libclang.dll is expected to be available in the PATH or in Emacs' exec-path.

* STANDARD INSTALLATION(Linux, Windows):
  Generate a Unix Makefile or a Visual Studio Project by CMake.

  - Self-Build step
    1. LLVM
       checkout, apply patch, generate project, build
       It is recommended that you use this shell.
       [https://github.com/yaruopooner/llvm-build-shells.git]

    2. Clang Server
       generate project, build

    see clang-server's reference manual.
    ac-clang/clang-server/readme.org

    sorry, reference manual is japanese version only.
    please help english version reference manual.

* NOTICE:
  - LLVM libclang.[dll, so, ...]
    this binary is not official binary.
    because offical libclang has mmap lock problem.
    applied a patch to LLVM's source code in order to solve this problem.

    see clang-server's reference manual.
    ac-clang/clang-server/readme.org

    sorry, reference manual is japanese version only.
    please help english version reference manual.



Usage:
* DETAILED MANUAL:
  For more information and detailed usage, refer to the project page:
  [https://github.com/yaruopooner/ac-clang]

* SETUP:
  (require 'ac-clang)

  (setq w32-pipe-read-delay 0)          ;; <- Windows Only

  (when (ac-clang-initialize)
    (add-hook 'c-mode-common-hook '(lambda ()
                                     (setq ac-clang-cflags CFLAGS)
                                     (ac-clang-activate-after-modify))))

* DEFAULT KEYBIND
  - start auto completion
    code completion & arguments expand
    `.` `->` `::`
  - start manual completion
    code completion & arguments expand
    `<tab>`
  - jump to definition / return from definition
    this is nestable jump.
    `M-.` / `M-,`
