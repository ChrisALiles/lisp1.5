package LISP15

import (
	"bytes"
	"testing"
	"time"
)

func TestLISP(t *testing.T) {

	var display bytes.Buffer

	// Channel between lexer and parser.
	var items = make(chan Item)
	// Start the parser in a separate goroutine.
	go Parser(items, &display)

	for _, te := range tests {
		t.Run(te.name, func(t *testing.T) {
			Lexer(te.lisp, items)
			// Allow time for the LISP response.
			// Is there a better way to do this?
			time.Sleep(time.Duration(2 * int(time.Millisecond)))
			// Remove prompt (suffix) and error prefix.
			dd := bytes.TrimSuffix(display.Bytes(), []byte("\nLISP>>"))
			d := string(bytes.TrimPrefix(dd, []byte("ERROR **  ")))
			if d != te.expect {
				t.Errorf("expected %v: received %v", te.expect, d)
			}
			display.Reset()
		})
	}
}

type test struct {
	name   string
	expect string
	lisp   string
}

var tests = []test{
	{"car OK", "a", "(car '(a b c))"},
	{"car nil", "NIL", "(car '())"},
	{"car NOK", "car argument must be a list", "(car 'a)"},
	{"car arg", "too few arguments", "(car)"},
	{"var noval", "variable \"p\" has no value", "p"},
	{"setq", "41", "(setq p 41)"},
	{"var val", "41", "p"},
	{"setq args 1", "too few arguments", "(setq)"},
	{"setq args 2", "too few arguments", "(setq 'a)"},
	{"cons OK", "(a b c)", "(cons 'a '(b c))"},
	{"cons nil", "(NIL)", "(cons '() '())"},
	{"cons NOK", "cons second argument must be a list", "(cons 'a 'b)"},
	{"cons args 1", "too few arguments", "(cons 'a)"},
	{"cons args 2", "too few arguments", "(cons)"},
	{"cdr OK", "(b c d e)", "(cdr '(a b c d e))"},
	{"cdr nil", "NIL", "(cdr '())"},
	{"cdr NOK", "cdr argument must be a list", "(cdr 'b)"},
	{"cdr arg", "too few arguments", "(cdr)"},
	{"list", "(1 2 3)", "(list 1 2 3)"},
	{"list no arg", "NIL", "(list)"},
	{"eq Y", "T", "(eq 42 42)"},
	{"eq N", "NIL", "(eq 'a 'b)"},
	{"eq args 1", "too few arguments", "(eq)"},
	{"eq args 2", "too few arguments", "(eq 'a)"},
	{"null Y", "T", "(null '())"},
	{"null N", "NIL", "(null 'a)"},
	{"null arg", "too few arguments", "(null)"},
	{"atom Y", "T", "(atom 'a)"},
	{"atom nil Y", "T", "(atom '())"},
	{"atom N", "NIL", "(atom '(1))"},
	{"atom arg", "too few arguments", "(atom)"},
	{"quote", "hello", "(quote hello)"},
	{"quote arg", "too few arguments", "(quote)"},
	{"define", "define is not implemented", "(define 23)"},
	{"defun sq def", "sq", "(defun sq lambda(x) (times x x))"},
	{"defun sq run 1", "100", "(sq 10)"},
	{"defun sq run 2", "400", "(sq 20)"},
	{"defun fact def", "fact", "(defun fact lambda(n) (cond ((zerop n) 1) (t (times n (fact (sub1 n))))))"},
	{"defun fact run 1", "6", "(fact 3)"},
	{"defun fact run 2", "3628800", "(fact 10)"},
	{"defun args", "too few arguments", "(defun)"},
	{"plus OK", "42", "(plus 21 9 12)"},
	{"plus NOK", "PLUS argument \"z\" is not a number", "(plus 1 'z)"},
	{"plus args", "too few arguments", "(plus)"},
	{"diff OK", "12", "(diff 42 30)"},
	{"diff -ve OK", "-10", "(diff 20 30)"},
	{"diff NOK 1", "too few arguments", "(diff 2)"},
	{"diff NOK 2", "DIFFERENCE argument \"z\" is not a number", "(diff 'z 2)"},
	{"diff NOK 3", "DIFFERENCE argument \"w\" is not a number", "(diff 2 'w)"},
	{"times OK", "100", "(times 2 50)"},
	{"times NOK 1", "TIMES argument \"z\" is not a number", "(times 10 'z)"},
	{"times NOK 2", "TIMES argument \"z\" is not a number", "(times 'z 10)"},
	{"times args", "too few arguments", "(times)"},
	{"add1 OK", "10", "(add1 9)"},
	{"add1 NOK", "ADD1 argument \"z\" is not a number", "(add1 'z)"},
	{"{add1 arg", "too few arguments", "(add1)"},
	{"sub1 OK", "41", "(sub1 42)"},
	{"sub1 NOK", "SUB1 argument \"z\" is not a number", "(sub1 'z)"},
	{"sub1 arg", "too few arguments", "(sub1)"},
	{"zerop OK 1", "T", "(zerop 0)"},
	{"zerop OK 2", "T", "(zerop 0.0)"},
	{"zerop NOK", "ZEROP argument \"z\" is not a number", "(zerop 'z)"},
	{"zerop arg", "too few arguments", "(zerop)"},
	{"mapcar fn 1 list", "(2 3 4)", "(mapcar 'add1 '(1 2 3))"},
	{"mapcar fn 2 lists", "(6 8 10 12)", "(mapcar 'plus '(1 2 3 4) '(5 6 7 8))"},
	{"mapcar fn 2 lists uneven", "(6 8 10)", "(mapcar 'plus '(1 2 3 4) '(5 6 7))"},
	{"mapcar fn NOK", "error calling mapcar function - PLUS argument \"b\" is not a number", "(mapcar 'plus '(1 2 3 4) '(5 6b 7 8))"},
	{"mapcar lambda 1 list", "(2 3 4)", "(mapcar (lambda(n) (add1 n)) '(1 2 3))"},
	{"mapcar lambda 2 lists", "(6 8 10 12)", "(mapcar (lambda(x y) (plus x y)) '(1 2 3 4) '(5 6 7 8))"},
	{"mapcar lambda 2 lists uneven", "(6 8 10)", "(mapcar (lambda(x y) (plus x y)) '(1 2 3) '(5 6 7 8))"},
	{"mapcar lambda NOK 1", "LAMBDA expected but not found", "(mapcar (x y) (plus x y) '(1 2 3) '(4 5 6))"},
	{"mapcar lambda NOK 2", "mismatch lambda params and mapcar lists", "(mapcar (lambda(x y) (plus x y)) '(1 2 3))"},
	{"mapcar args", "too few arguments", "(mapcar)"},
	{"cond OK 1", "41", "(cond ((eq 1 2) 42) (t 41))"},
	{"cond OK 2", "42", "(cond ((eq 1 1) 42) (t 41))"},
	{"cond args", "too few arguments", "(cond)"},
	{"unknown fn", "unknown function \"doit\"", "(doit 'now)"},
}
