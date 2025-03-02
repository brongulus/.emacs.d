;;;; Snippets

;; Check (info "(autotype) Skeleton Language")
;; https://www.lysator.liu.se/~davidk/elisp/tempo-examples.html
;; https://www.lysator.liu.se/~davidk/elisp/

;; (define-abbrev global-abbrev-table "shbng" "#!/usr/bin/env ")
(autoload 'tempo-define-template "tempo")
(autoload 'tempo-forward-mark "tempo")
(autoload 'tempo-expand-if-complete "tempo")
(autoload 'tempo-backward-mark "tempo")
(autoload 'tempo-use-tag-list "tempo")

(defvar emacs-lisp-tempo-tags nil)
(defvar eshell-tempo-tags nil)
(defvar perl-tempo-tags nil)
(defvar go-ts-tempo-tags nil)
(defvar rust-ts-tempo-tags nil)
(defvar zig-tempo-tags nil)

(defun tempo-tab () (interactive)
       (unless (tempo-forward-mark)
         (if (eq major-mode 'eshell-mode)
             (completion-at-point)
           (indent-for-tab-command))))

(defun tempo-space () (interactive)
       (unless (tempo-expand-if-complete)
         (insert " ")))

(defun setup-tempo-keys (mode-map)
  (define-key mode-map (kbd "SPC") #'tempo-space)
  (define-key mode-map (kbd "TAB") #'tempo-tab)
  (define-key mode-map (kbd "<backtab>") #'tempo-backward-mark))

(with-eval-after-load 'tempo
  (setq tempo-interactive nil)
;;; --Eshell/elisp-------------------------------------------------------
  (tempo-define-template "eshell-for"
                         '("for f in " p " { " p " \"$f\"; }")
                         "forl" "" 'eshell-tempo-tags)
  (tempo-define-template "int-lambda"
                         '("lambda nil (interactive)")
                         "lnint" "" 'emacs-lisp-tempo-tags)
;;; --Perl----------------------------------------------------------------
  (tempo-define-template "pl-header"
                         '("#!/usr/bin/env perl" n n
                           "use 5.016;" n
                           "use warnings;" n
                           "use autodie;" n n)
                         "plh" "" 'perl-tempo-tags)
;;; --Go------------------------------------------------------------------
  (tempo-define-template "go-test-err"
                         '(> "if got != want {" > n
                             "t.Errorf(\"got %d want %d given, %v\", got, want, " p ")" > n
                             "}" > n>
                             )
                         "goerr" "" 'go-ts-tempo-tags)
  (tempo-define-template "errnil"
                         '(> "if err != nil {" > n
                             >  p n "}" > n >
                             )
                         "errnil" "" 'go-ts-tempo-tags)
;;; --Rust------------------------------------------------------------------
  (tempo-define-template "rs-print"
                         '("println!(\"" p "\");")
                         "pln" "" 'rust-ts-tempo-tags)
  (tempo-define-template "rs-dbg"
                         '("dbg!(\"" p "\");")
                         "dbg" "" 'rust-ts-tempo-tags)
;;; --Zig-(src: matklad)----------------------------------------------------
  (tempo-define-template "zig-assert"
                         '("const assert = std.debug.assert;")
                         "iass" "" 'zig-tempo-tags)
  (tempo-define-template "zig-std"
                         '("const std = @import(\"std\");")
                         "istd" "" 'zig-tempo-tags)
  (tempo-define-template "zig-main"
                         '("pub fn main() !void {" > n
			               > p n
			               "}" > n >)
                         "zmain" "" 'zig-tempo-tags)
  (tempo-define-template "zig-err-log"
                         '("log.err(\"{}:" p "\", .{" p "});")
                         "lerr" "" 'zig-tempo-tags)
  (tempo-define-template "zig-deb-pln"
                         '("std.debug.print(\"{}\\n\", .{" p"});")
                         "pln" "" 'zig-tempo-tags))

(with-eval-after-load 'em-cmpl
  (setup-tempo-keys eshell-cmpl-mode-map))
(add-hook 'eshell-mode-hook
          (lambda nil (tempo-use-tag-list 'eshell-tempo-tags)))

(dolist (mode '(emacs-lisp perl go-ts rust-ts zig))
  (let ((hook (intern (concat (symbol-name mode) "-mode-hook")))
        (map (intern (concat (symbol-name mode) "-mode-map")))
        (tags (intern (concat (symbol-name mode) "-tempo-tags"))))
    (add-hook hook `(lambda nil
                      (setup-tempo-keys ,map)
                      (tempo-use-tag-list ',tags)))))

;; (define-skeleton rs-header "Base rust template for competitive programming." ""
;;   "use std::io::{self, prelude::*};\n\n"

;;   "fn solve<R: BufRead, W: Write>(scan: &mut Scanner<R>, w: &mut W) -> io::Result<()> {\n"
;;   "\t// cin == scan.token(); cout == writeln!(w, \"{ans}\")\n"
;;   "\t" _ "\n"
;;   "}\n\n"

;;   "fn main() -> io::Result<()> {\n"
;;   "\tlet (stdin, stdout) = (io::stdin(), io::stdout());\n"
;;   "\tlet mut scan = Scanner::new(io::BufReader::new(stdin.lock()));\n"
;;   "\tlet mut out = io::BufWriter::new(stdout.lock());\n"

;;   "\t(0..scan.token()).try_for_each(|_| solve(&mut scan, &mut out))\n"
;;   "}\n\n"

;;   "pub struct Scanner<R> {\n"
;;   "\treader: R,\n"
;;   "\tbuf_str: Vec<u8>,\n"
;;   "\tbuf_iter: std::str::SplitAsciiWhitespace<'static>,\n"
;;   "}\n\n"

;;   "impl<R: std::io::BufRead> Scanner<R> {\n"
;;   "\tpub fn new(reader: R) -> Self {\n"
;;   "\t\tSelf {\n"
;;   "\t\t\treader,\n"
;;   "\t\t\tbuf_str: vec![],\n"
;;   "\t\t\tbuf_iter: \"\".split_ascii_whitespace(),\n"
;;   "\t\t}\n"
;;   "\t}\n\n"

;;   "\tpub fn token<T: std::str::FromStr>(&mut self) -> T {\n"
;;   "\t\tloop {\n"
;;   "\t\t\tif let Some(token) = self.buf_iter.next() {\n"
;;   "\t\t\t\treturn token.parse().ok().expect(\"Failed parse\");\n"
;;   "\t\t\t}\n"
;;   "\t\t\tself.buf_str.clear();\n"
;;   "\t\t\tself.reader\n"
;;   "\t\t\t\t.read_until(b'\\n', &mut self.buf_str)\n"
;;   "\t\t\t\t.expect(\"Failed read\");\n"
;;   "\t\t\tself.buf_iter = unsafe {\n"
;;   "\t\t\t\tlet slice = std::str::from_utf8_unchecked(&self.buf_str);\n"
;;   "\t\t\t\tstd::mem::transmute(slice.split_ascii_whitespace())\n"
;;   "\t\t\t}\n"
;;   "\t\t}\n"
;;   "\t}\n"
;;   "}")


;;; --C++------------------------------------------------------------------
;; (define-skeleton cpp-header "Base c++ template for competitive programming." ""
;;   "#include<bits/stdc++.h>\n"
;;   "using namespace std;\n"
;;   "\n#ifdef LOCAL\n"
;;   "#include \"algo/debug.h\"\n"
;;   "#else\n"
;;   "#define debug(...) 42\n"
;;   "#endif\n"
;;   "\nint main () {\n"
;;   "\tios::sync_with_stdio(0);\n"
;;   "\tcin.tie(0);\n"
;;   "\t" _ "\n"
;;   "}")

;; (define-skeleton cpp-for-loop
;;   "Insert a C++ for loop with user-defined iterator and termination variable." ""
;;   > "for (int " (setq iterator (read-char "Iterator variable: ")) " = 0; " iterator " < "
;;   > (read-char "Termination: ") "; " iterator "++) {\n"
;;   > _ "\n}" >)

;; (define-skeleton sortl "Sort with custom comparator." ""
;;   > "std::sort(v.begin(), v.end(), [](auto &left, auto &right) {\n\t"
;;   > "return " _ "left.second < right.second;\n"
;;   > "});")

;; (define-skeleton cpp-tests "Run multiple testcases." ""
;;   > "int tt = 0; cin >> tt;\n\t"
;;   > "while(tt--) {\n\t\t"
;;   > _ "\n}" >)

;; (define-skeleton cpp-all "Run from beginning to end of iterator." ""
;;   > (setq var (skeleton-read "Iterator variable: ")) ".begin(), " var ".end()"
;;   > (forward-char)
;;   > _)

;; (defun init-c++-abbrevs ()
;;   (define-abbrev c++-mode-abbrev-table "gtc" "" 'cpp-header)
;;   (define-abbrev c++-mode-abbrev-table "forl" "" 'cpp-for-loop)
;;   (define-abbrev c++-mode-abbrev-table "all" "" 'cpp-all)
;;   (define-abbrev c++-mode-abbrev-table "ttt" "" 'cpp-tests))
;; (defun init-c++-ts-abbrevs ()
;;   (define-abbrev c++-ts-mode-abbrev-table "gtc" "" 'cpp-header)
;;   (define-abbrev c++-ts-mode-abbrev-table "forl" "" 'cpp-for-loop)
;;   (define-abbrev c++-ts-mode-abbrev-table "all" "" 'cpp-all)
;;   (define-abbrev c++-ts-mode-abbrev-table "ttt" "" 'cpp-tests))
;; (add-hook 'c++-mode-hook 'abbrev-mode)
;; (add-hook 'c++-ts-mode-hook 'abbrev-mode)
;; (add-hook 'c++-mode-hook 'init-c++-abbrevs)
;; (add-hook 'c++-ts-mode-hook 'init-c++-ts-abbrevs)
