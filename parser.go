package LISP15

import (
	"errors"
	"fmt"
	"io"
)

var errEndList = errors.New("mismatched right paren")

func Parser(items chan Item, display io.Writer) {
	for {
		sxpr, err := getSexpr(items)
		if err != nil {
			fmt.Fprintln(display, "ERROR ** ", err)
			fmt.Fprint(display, Prompt)
			continue
		}
		finalSxpr, err := evalQuote(sxpr)
		if err != nil {
			fmt.Fprintln(display, "ERROR ** ", err)
			fmt.Fprint(display, Prompt)
			continue
		}
		fmt.Fprintln(display, finalSxpr)
		fmt.Fprint(display, Prompt)
	}

}
func getSexpr(items chan Item) (sexpr, error) {

	var sxpr sexpr
	item := <-items

	if item.typ == itemQuoteMark {
		sxpr.Quoted = true
		item = <-items
	}

	if item.typ == itemRightParen {
		return sxpr, errEndList
	}

	if item.typ != itemLeftParen {
		sxpr.SexprTyp = atomSexpr
		sxpr.AtomName = item.val
		sxpr.Val = item.val
		if item.typ == itemNumber {
			sxpr.Quoted = true
		}
		return sxpr, nil
	}
	sxpr.SexprTyp = listSexpr
	sxpr.List = make([]sexpr, 0, 10)

	for {
		subSxpr, err := getSexpr(items)

		// If the list is quoted, so are the elements.
		if sxpr.Quoted {
			subSxpr.Quoted = true
		}

		if err == errEndList {
			return sxpr, nil
		}
		if err != nil {
			return sxpr, err
		}
		sxpr.List = append(sxpr.List, subSxpr)
	}
}
