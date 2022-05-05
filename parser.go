package LISP15

import (
	"errors"
	"fmt"
)

var errEndList = errors.New("mismatched right paren")

func Parser(items chan Item) {
	for {
		sxpr, err := getSexpr(items)
		if err != nil {
			fmt.Println("ERROR ** ", err)
			fmt.Print(Prompt)
			continue
		}
		finalSxpr, err := evalQuote(sxpr)
		if err != nil {
			fmt.Println("ERROR ** ", err)
			fmt.Print(Prompt)
			continue
		}
		fmt.Println(finalSxpr)
		fmt.Print(Prompt)
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
