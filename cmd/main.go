package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/ChrisALiles/LISP15"
)

func main() {

	fmt.Println("LISP 1.5 (partial)")

	// Load user defined function definitions.
	err := LISP15.Load()
	if err != nil {
		fmt.Println("Saved functions were not loaded")
		fmt.Println(err)
	}

	// Channel between lexer and parser.
	items := make(chan LISP15.Item)

	// Start the parser in a separate goroutine.
	go LISP15.Parser(items, os.Stdout)

	fmt.Print(LISP15.Prompt)

	// The read eval print loop.
	// The lexer runs in the main goroutine, the parser
	// in the other.
	for {
		scanner := bufio.NewScanner(os.Stdin)
		scanner.Scan()
		// This LISP is case neutral - convert everything
		// to lower for consistency.
		sl := strings.ToLower(scanner.Text())
		LISP15.Lexer(sl, items)
	}
}
