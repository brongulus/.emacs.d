;;;; Snippets

;; Check (info "(autotype) Skeleton Language")
;; https://www.lysator.liu.se/~davidk/elisp/tempo-examples.html
;; https://www.lysator.liu.se/~davidk/elisp/

;;; --Perl----------------------------------------------------------------
(define-skeleton pl-header "Perl header." ""
  "#!/usr/bin/env perl\n\n"
  "use 5.016;\n"
  "use warnings;\n"
  "use autodie;\n")

(add-hook 'perl-mode-hook 'abbrev-mode)
(add-hook 'perl-mode-hook
          (lambda nil
            (define-abbrev perl-mode-abbrev-table "plh" "" 'pl-header)))

;;; --Go------------------------------------------------------------------
(define-skeleton go-err "Go test check." ""
  > "if got != want {\n"
  > "t.Errorf(\"got %d want %d given, %v\", got, want, " _ ")\n}" >)

(define-skeleton errnil "Go check err." ""
  > "if err != nil {\n"
  >  _ "\n}" >)

(add-hook 'go-ts-mode-hook 'abbrev-mode)
(add-hook 'go-ts-mode-hook
          (lambda ()
            (define-abbrev go-ts-mode-abbrev-table "goerr" "" 'go-err)
            (define-abbrev go-ts-mode-abbrev-table "errnil" "" 'errnil)))

;;; --Rust------------------------------------------------------------------
(define-skeleton rs-print "Faster println! call." ""
  "println!(\"" _ "\");")

(define-skeleton rs-dbg "Faster dbg! call." ""
  "dbg!(\"" _ "\");")

(define-skeleton rs-header "Base rust template for competitive programming." ""
  "use std::io::{self, prelude::*};\n\n"

  "fn solve<R: BufRead, W: Write>(scan: &mut Scanner<R>, w: &mut W) -> io::Result<()> {\n"
  "\t// cin == scan.token(); cout == writeln!(w, \"{ans}\")\n"
  "\t" _ "\n"
  "}\n\n"
  
  "fn main() -> io::Result<()> {\n"
  "\tlet (stdin, stdout) = (io::stdin(), io::stdout());\n"
  "\tlet mut scan = Scanner::new(io::BufReader::new(stdin.lock()));\n"
  "\tlet mut out = io::BufWriter::new(stdout.lock());\n"
  
  "\t(0..scan.token()).try_for_each(|_| solve(&mut scan, &mut out))\n"
  "}\n\n"
  
  "pub struct Scanner<R> {\n"
  "\treader: R,\n"
  "\tbuf_str: Vec<u8>,\n"
  "\tbuf_iter: std::str::SplitAsciiWhitespace<'static>,\n"
  "}\n\n"
  
  "impl<R: std::io::BufRead> Scanner<R> {\n"
  "\tpub fn new(reader: R) -> Self {\n"
  "\t\tSelf {\n"
  "\t\t\treader,\n"
  "\t\t\tbuf_str: vec![],\n"
  "\t\t\tbuf_iter: \"\".split_ascii_whitespace(),\n"
  "\t\t}\n"
  "\t}\n\n"
  
  "\tpub fn token<T: std::str::FromStr>(&mut self) -> T {\n"
  "\t\tloop {\n"
  "\t\t\tif let Some(token) = self.buf_iter.next() {\n"
  "\t\t\t\treturn token.parse().ok().expect(\"Failed parse\");\n"
  "\t\t\t}\n"
  "\t\t\tself.buf_str.clear();\n"
  "\t\t\tself.reader\n"
  "\t\t\t\t.read_until(b'\\n', &mut self.buf_str)\n"
  "\t\t\t\t.expect(\"Failed read\");\n"
  "\t\t\tself.buf_iter = unsafe {\n"
  "\t\t\t\tlet slice = std::str::from_utf8_unchecked(&self.buf_str);\n"
  "\t\t\t\tstd::mem::transmute(slice.split_ascii_whitespace())\n"
  "\t\t\t}\n"
  "\t\t}\n"
  "\t}\n"
  "}")

(add-hook 'rust-ts-mode-hook 'abbrev-mode)
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (define-abbrev rust-ts-mode-abbrev-table "gtc" "" 'rs-header)
            (define-abbrev rust-ts-mode-abbrev-table "pln" "" 'rs-print)
            (define-abbrev rust-ts-mode-abbrev-table "dbg" "" 'rs-dbg)))

;;; --C++------------------------------------------------------------------
(define-skeleton cpp-header "Base c++ template for competitive programming." ""
  "#include<bits/stdc++.h>\n"
  "using namespace std;\n"
  "\n#ifdef LOCAL\n"
  "#include \"algo/debug.h\"\n"
  "#else\n"
  "#define debug(...) 42\n"
  "#endif\n"
  "\nint main () {\n"
  "\tios::sync_with_stdio(0);\n"
  "\tcin.tie(0);\n"
  "\t" _ "\n"
  "}")

(define-skeleton cpp-for-loop
  "Insert a C++ for loop with user-defined iterator and termination variable." ""
  > "for (int " (setq iterator (read-char "Iterator variable: ")) " = 0; " iterator " < "
  > (read-char "Termination: ") "; " iterator "++) {\n"
  > _ "\n}" >)

(define-skeleton sortl "Sort with custom comparator." ""
  > "std::sort(v.begin(), v.end(), [](auto &left, auto &right) {\n\t"
  > "return " _ "left.second < right.second;\n"
  > "});")

(define-skeleton cpp-tests "Run multiple testcases." ""
  > "int tt = 0; cin >> tt;\n\t"
  > "while(tt--) {\n\t\t"
  > _ "\n}" >)

(define-skeleton cpp-all "Run from beginning to end of iterator." ""
  > (setq var (skeleton-read "Iterator variable: ")) ".begin(), " var ".end()"
  > (forward-char)
  > _)

(defun init-c++-abbrevs ()
  (define-abbrev c++-mode-abbrev-table "gtc" "" 'cpp-header)
  (define-abbrev c++-mode-abbrev-table "forl" "" 'cpp-for-loop)
  (define-abbrev c++-mode-abbrev-table "all" "" 'cpp-all)
  (define-abbrev c++-mode-abbrev-table "ttt" "" 'cpp-tests))
(defun init-c++-ts-abbrevs ()
  (define-abbrev c++-ts-mode-abbrev-table "gtc" "" 'cpp-header)
  (define-abbrev c++-ts-mode-abbrev-table "forl" "" 'cpp-for-loop)
  (define-abbrev c++-ts-mode-abbrev-table "all" "" 'cpp-all)
  (define-abbrev c++-ts-mode-abbrev-table "ttt" "" 'cpp-tests))
(add-hook 'c++-mode-hook 'abbrev-mode)
(add-hook 'c++-ts-mode-hook 'abbrev-mode)
(add-hook 'c++-mode-hook 'init-c++-abbrevs)
(add-hook 'c++-ts-mode-hook 'init-c++-ts-abbrevs)

