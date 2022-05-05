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

	err := LISP15.Load()
	if err != nil {
		fmt.Println("Saved functions were not loaded")
		fmt.Println(err)
	}

	items := make(chan LISP15.Item)

	go LISP15.Parser(items)

	fmt.Print(LISP15.Prompt)

	for {
		scanner := bufio.NewScanner(os.Stdin)
		scanner.Scan()
		// This LISP is case neutral - convert everything
		// to lower for consistency.
		sl := strings.ToLower(scanner.Text())
		LISP15.Lexer(sl, items)
	}
}
