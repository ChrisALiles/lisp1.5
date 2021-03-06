package LISP15

import (
	"fmt"
	"os"
	"reflect"
	"strconv"
)

// Evaluate variables/parameters and apply functions.
func evalQuote(sxpr sexpr) (sexpr, error) {

	if sxpr.Quoted {
		return sxpr, nil
	}

	if sxpr.SexprTyp == atomSexpr {
		return evalAtom(sxpr)
	}

	// Empty list.
	if len(sxpr.List) == 0 {
		return nilSexpr, nil
	}

	// debugging.
	// Add a new environment frame.
	if sxpr.List[0].AtomName == "pushf" {
		environ.pushFrame()
		return nilSexpr, nil
	}

	// debugging
	// Remove the most recent environment frame.
	if sxpr.List[0].AtomName == "popf" {
		environ.popFrame()
		return nilSexpr, nil
	}

	// Check the number of args supplied.
	num, found := minArgs[sxpr.List[0].AtomName]
	if found && num > (len(sxpr.List)-1) {
		return sxpr, fmt.Errorf("too few arguments")
	}

	// The next few fuctions are those that don't
	// need to have their parameters evaluated
	// (immediately).
	if sxpr.List[0].AtomName == "cond" {
		return evalCond(sxpr)
	}

	if sxpr.List[0].AtomName == "quit" {
		return applyQuit(sxpr)
	}

	if sxpr.List[0].AtomName == "defun" {
		return applyDefun(sxpr)
	}

	if sxpr.List[0].AtomName == "save" {
		return nilSexpr, save()
	}

	if sxpr.List[0].AtomName == "load" {
		return nilSexpr, Load()

	}

	if sxpr.List[0].AtomName == "setq" || sxpr.List[0].AtomName == "quote" {
		sxpr.List[1].Quoted = true
	}

	// Dont't evaluate args for mapcar lambda.
	if sxpr.List[0].AtomName == "mapcar" && sxpr.List[1].SexprTyp == listSexpr {
		return applyMapcarLambda(sxpr)
	}

	// Evaluate parameters.
	for indx, thisSxpr := range sxpr.List[1:] {

		if thisSxpr.Quoted {
			continue
		}
		result, err := evalQuote(thisSxpr)
		if err != nil {
			return sxpr, err
		}
		sxpr.List[indx+1] = result
	}

	// Temporary work around for problem putting
	// mapcar in the property list (issue #52860).
	// 12 May 2022 - this code will have to stay - the
	// Go compiler is working as intended.
	if sxpr.List[0].AtomName == "mapcar" {
		return applyMapcar(sxpr)
	}

	// The property list has the names of the apply
	// functions for the various LISP functions.
	prop, found := propList[sxpr.List[0].AtomName]
	if found && prop.Subr != nil {
		// Call the apply function.
		return prop.Subr(sxpr)
	}

	// Now check if it's a user defined function.
	fSxpr, found := environ.lookUp(sxpr.List[0].AtomName)

	if !found {
		err := fmt.Errorf("unknown function %q", sxpr.List[0].AtomName)
		return sxpr, err
	}
	// If the returned expression is used, the original
	// function gets overwritten by run time parameter
	// values because (presumably) slice pointers still
	// lead back to the function definition in the
	// associatopnList map.
	// 23 May 2022 - now ot looks to me that the code
	// is working as one would expect.
	// The duplicate function removes all the
	// myriad slice entanglements and the resulting
	// expression can then be safely worked on.
	fnSxpr, err := fSxpr.duplicate()
	if err != nil {
		return sxpr, err
	}

	// Create a new environment frame in which to resolve
	// parameter values.
	environ.pushFrame()

	defer func() {
		environ.popFrame()
	}()

	// Bind the calling values to the formal parameter
	// names.
	for index, param := range fnSxpr.List[1].List {
		environ.store(param.AtomName, sxpr.List[index+1])
	}

	// Now execute the function body - presumably this
	// could be a number of S-expressions.
	// Note that the new environment frame is still active.
	var fnResult sexpr
	var fnErr error
	for _, exSxpr := range fnSxpr.List[2:] {
		fnResult, fnErr = evalQuote(exSxpr)
		if fnErr != nil {
			return sxpr, fnErr
		}
	}
	return fnResult, nil
}

// End the REPL session.
func applyQuit(sxpr sexpr) (sexpr, error) {

	fmt.Println("Shutting down ...")
	err := save()
	if err != nil {
		fmt.Println("Function definitions not saved")
		fmt.Println(err)
	}
	os.Exit(0)

	return sxpr, nil
}

func applySetq(sxpr sexpr) (sexpr, error) {

	environ.store(sxpr.List[1].AtomName, sxpr.List[2])

	return sxpr.List[2], nil
}

func applyCons(sxpr sexpr) (sexpr, error) {

	// This LISP doen't (initially) know the dot notation
	// so the second argument of cons must be a list.
	if sxpr.List[2].SexprTyp == atomSexpr {
		err := fmt.Errorf("cons second argument must be a list")
		return sxpr, err
	}
	// Need the first arg as a slice, then append to it
	// the members of the second arg - with ...
	var retSexpr = sexpr{
		listSexpr, false, "", "",
		append(sxpr.List[1:2], sxpr.List[2].List...),
	}
	return retSexpr, nil
}

func applyCar(sxpr sexpr) (sexpr, error) {

	if sxpr.List[1].SexprTyp == atomSexpr {
		err := fmt.Errorf("car argument must be a list")
		return sxpr, err
	}

	if len(sxpr.List[1].List) == 0 {
		return nilSexpr, nil
	}

	return sxpr.List[1].List[0], nil
}

func applyCdr(sxpr sexpr) (sexpr, error) {

	if sxpr.List[1].SexprTyp == atomSexpr {
		err := fmt.Errorf("cdr argument must be a list")
		return sxpr, err
	}

	if len(sxpr.List[1].List) == 0 {
		return nilSexpr, nil
	}

	var retSexpr = sexpr{
		listSexpr, false, "", "", sxpr.List[1].List[1:],
	}
	return retSexpr, nil
}

func applyList(sxpr sexpr) (sexpr, error) {

	var retSexpr = sexpr{
		listSexpr, false, "", "", sxpr.List[1:],
	}
	return retSexpr, nil
}

func applyEq(sxpr sexpr) (sexpr, error) {

	if reflect.DeepEqual(sxpr.List[1], sxpr.List[2]) {
		return trueSexpr, nil
	}
	return nilSexpr, nil
}

func applyNull(sxpr sexpr) (sexpr, error) {

	if sxpr.List[1].SexprTyp == listSexpr &&
		len(sxpr.List[1].List) == 0 {
		return trueSexpr, nil
	}
	return nilSexpr, nil
}

func applyAtom(sxpr sexpr) (sexpr, error) {

	// The empty list is also an atom.
	if sxpr.List[1].SexprTyp == atomSexpr ||
		len(sxpr.List[1].List) == 0 {
		return trueSexpr, nil
	}
	return nilSexpr, nil
}

func applyQuote(sxpr sexpr) (sexpr, error) {

	return sxpr.List[1], nil
}

// We use defun instead of define, at least initially.
func applyDefine(sxpr sexpr) (sexpr, error) {

	return sxpr, fmt.Errorf("define is not implemented")
}

func applyDefun(sxpr sexpr) (sexpr, error) {

	saveSxpr := sxpr
	// Save everything after defun name - lambda is
	// saved so we can tell it's a function later.
	saveSxpr.List = saveSxpr.List[2:]
	saveSxpr.AtomName = sxpr.List[1].AtomName

	environ.store(saveSxpr.AtomName, saveSxpr)

	// The return value is the function name.
	var returnSxpr sexpr
	returnSxpr.SexprTyp = atomSexpr
	returnSxpr.Val = saveSxpr.AtomName

	return returnSxpr, nil
}

var notNumErr = "%v argument %q is not a number"

func applyPlus(sxpr sexpr) (sexpr, error) {

	var sum float64
	for _, thisSxpr := range sxpr.List[1:] {
		num, err := strconv.ParseFloat(thisSxpr.Val, 64)
		if err != nil {
			genErr := fmt.Errorf(notNumErr, "PLUS", thisSxpr)
			return thisSxpr, genErr
		}
		sum += num
	}
	var result sexpr
	result.SexprTyp = atomSexpr
	result.Quoted = true
	result.Val = strconv.FormatFloat(sum, 'f', -1, 64)

	return result, nil
}

func applyDiff(sxpr sexpr) (sexpr, error) {

	if len(sxpr.List) != 3 {
		err := fmt.Errorf("DIFFERENCE requires 2 numeric arguments")
		return sxpr, err
	}

	num1, err := strconv.ParseFloat(sxpr.List[1].Val, 64)
	if err != nil {
		genErr := fmt.Errorf(notNumErr, "DIFFERENCE", sxpr.List[1].Val)
		return sxpr, genErr
	}
	num2, err := strconv.ParseFloat(sxpr.List[2].Val, 64)
	if err != nil {
		genErr := fmt.Errorf(notNumErr, "DIFFERENCE", sxpr.List[2].Val)
		return sxpr, genErr
	}

	var diff float64 = num1 - num2

	var result sexpr
	result.SexprTyp = atomSexpr
	result.Quoted = true
	result.Val = strconv.FormatFloat(diff, 'f', -1, 64)

	return result, nil
}

func applyTimes(sxpr sexpr) (sexpr, error) {

	var product float64
	for index, thisSxpr := range sxpr.List[1:] {
		num, err := strconv.ParseFloat(thisSxpr.Val, 64)
		if err != nil {
			genErr := fmt.Errorf(notNumErr, "TIMES", thisSxpr)
			return thisSxpr, genErr
		}
		if index == 0 {
			product = num
		} else {
			product *= num
		}
	}
	var result sexpr
	result.SexprTyp = atomSexpr
	result.Quoted = true
	result.Val = strconv.FormatFloat(product, 'f', -1, 64)

	return result, nil
}

func applyAdd1(sxpr sexpr) (sexpr, error) {

	num, err := strconv.ParseFloat(sxpr.List[1].Val, 64)
	if err != nil {
		genErr := fmt.Errorf(notNumErr, "ADD1", sxpr.List[1])
		return sxpr.List[1], genErr
	}
	num++
	var result sexpr
	result.SexprTyp = atomSexpr
	result.Quoted = true
	result.Val = strconv.FormatFloat(num, 'f', -1, 64)

	return result, nil
}

func applySub1(sxpr sexpr) (sexpr, error) {

	num, err := strconv.ParseFloat(sxpr.List[1].Val, 64)
	if err != nil {
		genErr := fmt.Errorf(notNumErr, "SUB1", sxpr.List[1])
		return sxpr.List[1], genErr
	}
	num--
	var result sexpr
	result.SexprTyp = atomSexpr
	result.Quoted = true
	result.Val = strconv.FormatFloat(num, 'f', -1, 64)

	return result, nil
}

func applyZerop(sxpr sexpr) (sexpr, error) {

	num, err := strconv.ParseFloat(sxpr.List[1].Val, 64)
	if err != nil {
		genErr := fmt.Errorf(notNumErr, "ZEROP", sxpr.List[1])
		return sxpr.List[1], genErr
	}
	if num != 0 {
		return nilSexpr, nil
	}
	return trueSexpr, nil
}

func applyMapcar(sxpr sexpr) (sexpr, error) {

	// The first argument should be either a function
	// name or (lambda.
	if sxpr.List[1].SexprTyp == listSexpr {
		applyMapcarLambda(sxpr)
	}

	var (
		fnCall sexpr // expression to call the function.
		result sexpr // mapcar list result.
	)
	fnCall.SexprTyp = listSexpr
	fnCall.List = make([]sexpr, 1, 10)
	fnCall.List[0].AtomName = sxpr.List[1].AtomName

	result.SexprTyp = listSexpr
	result.List = make([]sexpr, 0, 10)

elmtLoop:
	for elmtI := range sxpr.List[2].List {
		for _, list := range sxpr.List[2:] {
			if elmtI >= len(list.List) {
				break elmtLoop
			}
			fnCall.List = append(fnCall.List, list.List[elmtI])
		}
		resElmt, err := evalQuote(fnCall)
		if err != nil {
			return fnCall, fmt.Errorf("error calling mapcar function - %v", err)
		}
		fnCall.List = fnCall.List[:1]
		result.List = append(result.List, resElmt)
	}
	return result, nil
}

func applyMapcarLambda(sxpr sexpr) (sexpr, error) {

	if sxpr.List[1].List[0].AtomName != "lambda" {
		return sxpr, fmt.Errorf("LAMBDA expected but not found")
	}
	if len(sxpr.List[1].List[1].List) != len(sxpr.List)-2 {
		return sxpr, fmt.Errorf("mismatch lambda params and mapcar lists")
	}

	// Evaluate the lists (not the lambda part).
	for index, thisSxpr := range sxpr.List[2:] {
		evSxpr, err := evalQuote(thisSxpr)
		if err != nil {
			return thisSxpr, err
		}
		sxpr.List[index+2] = evSxpr
	}

	// Create a new environment frame in which to resolve
	// parameter values.
	environ.pushFrame()

	defer func() {
		environ.popFrame()
	}()

	var result sexpr
	result.SexprTyp = listSexpr
	result.List = make([]sexpr, 0, 10)

elmtLoop:
	for elmtI := range sxpr.List[2].List {
		for index, list := range sxpr.List[2:] {
			if elmtI >= len(list.List) {
				break elmtLoop
			}
			environ.store(sxpr.List[1].List[1].List[index].AtomName, sxpr.List[index+2].List[elmtI])
		}
		var (
			resElmt sexpr
			exSxpr  sexpr
			err     error
		)
		for _, thisSxpr := range sxpr.List[1].List[2:] {
			// We must execute copies of the s-expressions,
			// otherwise the formal parameters are over-written
			// on the first pass and never changed after that.
			exSxpr, err = thisSxpr.duplicate()
			if err != nil {
				return thisSxpr, err
			}
			resElmt, err = evalQuote(exSxpr)
			if err != nil {
				return thisSxpr, fmt.Errorf("error calling mapcar function - %v", err)
			}
		}
		result.List = append(result.List, resElmt)
	}
	return result, nil
}

func evalCond(sxpr sexpr) (sexpr, error) {

	for _, thisSxpr := range sxpr.List[1:] {
		result, err := evalQuote(thisSxpr.List[0])
		if err != nil {
			return thisSxpr, err
		}
		if reflect.DeepEqual(result, trueSexpr) {
			return evalQuote(thisSxpr.List[1])
		}
	}

	return nilSexpr, nil

}

func evalAtom(sxpr sexpr) (sexpr, error) {

	if sxpr.Quoted {
		return sxpr, nil
	}
	if sxpr.AtomName == trueVal {
		return trueSexpr, nil
	}
	if sxpr.AtomName == nilVal {
		return nilSexpr, nil
	}

	exSxpr, found := environ.lookUp(sxpr.AtomName)

	if !found {
		err := fmt.Errorf("variable %q has no value", sxpr.AtomName)
		return sxpr, err
	}

	return exSxpr, nil
}
