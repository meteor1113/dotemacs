;;; cargo-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cargo" "cargo.el" (0 0 0 0))
;;; Generated autoloads from cargo.el

(autoload 'cargo-minor-mode "cargo" "\
Cargo minor mode. Used to hold keybindings for cargo-mode.

This is a minor mode.  If called interactively, toggle the `cargo
minor mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `cargo-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{cargo-minor-mode-command-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "cargo" '("cargo-"))

;;;***

;;;### (autoloads nil "cargo-process" "cargo-process.el" (0 0 0 0))
;;; Generated autoloads from cargo-process.el

(autoload 'cargo-process-bench "cargo-process" "\
Run the Cargo bench command.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks." t nil)

(autoload 'cargo-process-build "cargo-process" "\
Run the Cargo build command.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project." t nil)

(autoload 'cargo-process-clean "cargo-process" "\
Run the Cargo clean command.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory." t nil)

(autoload 'cargo-process-doc "cargo-process" "\
Run the Cargo doc command.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation." t nil)

(autoload 'cargo-process-doc-open "cargo-process" "\
Run the Cargo doc command with the --open switch.
With the prefix argument, modify the command's invocation.
Cargo: Open this project's documentation." t nil)

(autoload 'cargo-process-new "cargo-process" "\
Run the Cargo new command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project.

\(fn NAME &optional BIN)" t nil)

(autoload 'cargo-process-init "cargo-process" "\
Run the Cargo init command.
With the prefix argument, modify the command's invocation.
DIRECTORY is the directory you want to create a cargo project in.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project in current directory.

DIRECTORY is created if necessary.

\(fn DIRECTORY &optional BIN)" t nil)

(autoload 'cargo-process-run "cargo-process" "\
Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs." t nil)

(autoload 'cargo-process-run-bin "cargo-process" "\
Run the Cargo run command --bin <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute a specific binary

\(fn COMMAND)" t nil)

(autoload 'cargo-process-run-example "cargo-process" "\
Run the Cargo run command --example <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute with --example <name>.

\(fn COMMAND)" t nil)

(autoload 'cargo-process-search "cargo-process" "\
Run the Cargo search command.
With the prefix argument, modify the command's invocation.
SEARCH-TERM is used as the search term for the Cargo registry.
Cargo: Search registry for crates.

\(fn SEARCH-TERM)" t nil)

(autoload 'cargo-process-test "cargo-process" "\
Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-current-test "cargo-process" "\
Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-current-file-tests "cargo-process" "\
Run the Cargo test command for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil)

(autoload 'cargo-process-update "cargo-process" "\
Run the Cargo update command.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock." t nil)

(autoload 'cargo-process-fmt "cargo-process" "\
Run the Cargo fmt command.
With the prefix argument, modify the command's invocation.
Requires rustfmt to be installed." t nil)

(autoload 'cargo-process-outdated "cargo-process" "\
Run the Cargo outdated command.
With the prefix argument, modify the command's invocation.
Requires cargo-outdated to be installed." t nil)

(autoload 'cargo-process-check "cargo-process" "\
Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed." t nil)

(autoload 'cargo-process-clippy "cargo-process" "\
Run the Cargo clippy command.
With the prefix argument, modify the command's invocation.
Cargo: Clippy compile the current project.
Requires clippy to be installed." t nil)

(autoload 'cargo-process-add "cargo-process" "\
Run the Cargo add command.
With the prefix argument, modify the command's invocation.
CRATES is the name of the crate to add.
Cargo: This command allows you to add a dependency to a Cargo.toml manifest file.

\(fn CRATE)" t nil)

(autoload 'cargo-process-audit "cargo-process" "\
Run the Cargo audit command.
With the prefix argument, modify the command's invocation.
Cargo: Audit checks the current project's Cargo.lock for security vulnerabilities.
Requires cargo-audit to be installed." t nil)

(autoload 'cargo-process-script "cargo-process" "\
Run the Cargo script command.
With the prefix argument, modify the command's invocation.
Cargo: Script compiles and runs 'Cargoified Rust scripts'.
Requires cargo-script to be installed." t nil)

(autoload 'cargo-process-watch "cargo-process" "\
Run the Cargo watch command.
With the prefix argument, modify the command's invocation.
Cargo: Watches over your Cargo project’s source.
Requires cargo-watch to be installed." t nil)

(autoload 'cargo-process-watch-run "cargo-process" "\
Run the Cargo watch command.
With the prefix argument, modify the command's invocation.
Cargo: Watches over your Cargo project’s source.
Requires cargo-watch to be installed." t nil)

(autoload 'cargo-process-rm "cargo-process" "\
Run the Cargo rm command.
With the prefix argument, modify the command's invocation.
CRATE is the name of the crate to remove.
Cargo: Remove a dependency from a Cargo.toml manifest file.

\(fn CRATE)" t nil)

(autoload 'cargo-process-upgrade "cargo-process" "\
Run the Cargo update command.
With the prefix argument, modify the command's invocation.
If ALL is t then update all crates, otherwise specify CRATES.
Cargo: Upgrade dependencies as specified in the local manifest file

\(fn &optional ALL CRATES)" t nil)

(autoload 'cargo-process-repeat "cargo-process" "\
Run the last cargo-process command." t nil)

(register-definition-prefixes "cargo-process" '("cargo-" "manifest-path-argument" "rustc-errno" "set-rust-backtrace"))

;;;***

;;;### (autoloads nil nil ("cargo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cargo-autoloads.el ends here
