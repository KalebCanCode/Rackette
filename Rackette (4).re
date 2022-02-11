open CS17SetupRackette;
open Read.Reader;
open Types;

//============================================================================
//============================================================================

/* plus: list(value) -> value
   Input: The input of plus is a list of values, alod. Plus should take in a
   list of two elements, both of which are NumV(int).
   Output: The output of plus is a NumV(int) resulting from adding the
   integers contained within the elements of the input list. If the list
   consists of more than two elements or contains something other than a
   NumV(int), the program will fail. */

let plus: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => NumV(x + y)
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(plus([NumV(1), NumV(-1)]), NumV(0), "plus-zero");
checkExpect(plus([NumV(1), NumV(1)]), NumV(2), "plus-two");
checkExpect(plus([NumV(-1), NumV(-1)]), NumV(-2), "plus-negative");
checkExpect(plus([NumV(1), NumV(0)]), NumV(1), "plus-positive");
checkError(() => plus([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => plus([NumV(0)]), "Must operate on two integers.");
checkError(() => plus([BoolV(true)]), "Must operate on two integers.");
checkError(() => plus([]), "Must operate on two integers.");
checkError(() => plus([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* minus: list(value) -> value
   Input: The input of minus is a list of values, alod. Minus should take in a
   list of two elements, both of which are NumV(int).
   Output: The output of plus is a NumV(int) resulting from subtracting the
   integers contained within the elements of the input list. If the list
   consists of more than two elements or contains something other than a
   NumV(int), the program will fail. */

let minus: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => NumV(x - y)
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(minus([NumV(1), NumV(1)]), NumV(0), "minus-zero");
checkExpect(minus([NumV(-1), NumV(-1)]), NumV(0), "minus-zero2");
checkExpect(minus([NumV(0), NumV(0)]), NumV(0), "minus-zero3");
checkExpect(minus([NumV(-3), NumV(8)]), NumV(-11), "minus-negative");
checkExpect(minus([NumV(8), NumV(3)]), NumV(5), "minus-positive");
checkError(() => minus([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => minus([NumV(0)]), "Must operate on two integers.");
checkError(() => minus([BoolV(true)]), "Must operate on two integers.");
checkError(() => minus([]), "Must operate on two integers.");
checkError(() => minus([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* multiply: list(value) -> value
   Input: The input of multiply is a list of values, alod. Multiply should
   take in a list of two elements, both of which are NumV(int).
   Output: The output of multiply is a NumV(int) resulting from multiplying
   the integers contained within the elements of the input list. If the list
   consists of more than two elements or contains something other than a
   NumV(int), the program will fail. */

let multiply: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => NumV(x * y)
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(multiply([NumV(0), NumV(18)]), NumV(0), "multiply-zero");
checkExpect(multiply([NumV(-0), NumV(18)]), NumV(0), "multiply-zero2");
checkExpect(multiply([NumV(1), NumV(18)]), NumV(18), "multiply-positive");
checkExpect(multiply([NumV(-1), NumV(-18)]),NumV(18),
  "multiply-positive2",);
checkExpect(multiply([NumV(-1), NumV(18)]), NumV(-18),
"multiply-negative",);
checkError(() => multiply([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => multiply([NumV(0)]), "Must operate on two integers.");
checkError(() => multiply([BoolV(true)]), "Must operate on two integers.");
checkError(() => multiply([]), "Must operate on two integers.");
checkError(() => multiply([ListV([NumV(3)])]),
"Must operate on two integers.",);

/* divide: list(value) -> value
   Input: The input of divide is a list of values, alod. Divide should take in
   a list of two elements, both of which are NumV(int).
   Output: The output of divide is a NumV(int) resulting from dividing the
   integers contained within the elements of the input list. If the list
   consists of more than two elements or contains something other than a
   NumV(int), the program will fail. The quotient is rounded down to the
   nearest integer. */

let divide: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => NumV(x / y)
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(divide([NumV(3), NumV(3)]), NumV(1), "divide-positive");
checkExpect(divide([NumV(3), NumV(2)]), NumV(1), "divide-positive2");
checkExpect(divide([NumV(-3), NumV(-2)]), NumV(1), "divide-positive3");
checkExpect(divide([NumV(0), NumV(1)]), NumV(0), "divide-zero");
checkExpect(divide([NumV(-8), NumV(2)]), NumV(-4), "divide-negative");
checkError(() => divide([NumV(3), NumV(0), NumV(6)]),
"Must operate on two integers.",);
checkError(() => divide([NumV(0)]), "Must operate on two integers.");
checkError(() => divide([BoolV(true)]), "Must operate on two integers.");
checkError(() => divide([]), "Must operate on two integers.");
checkError(() => divide([ListV([NumV(3)])]),
"Must operate on two integers.",);

/* remainder: list(value) -> value
   Input: The input of remainder is a list of values, alod. Remainder should
   take in a list of two elements, both of which are NumV(int).
   Output: The output of remainder is a NumV(int) containing the remainder of
   dividing the integers contained within the elements of the input list. If
   the list consists of more than two elements or contains something other
   than a NumV(int), the program will fail. */

let remainder: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => NumV(x - x / y * y)
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(remainder([NumV(3), NumV(2)]), NumV(1), "remainder-positive");
checkExpect(remainder([NumV(720), NumV(721)]), NumV(720),
  "remainder-positive2",);
checkExpect(remainder([NumV(720), NumV(3)]), NumV(0),
  "remainder-positive 3",);
checkExpect(remainder([NumV(0), NumV(1)]), NumV(0), "remainder-zero");
checkExpect(remainder([NumV(-3), NumV(1)]), NumV(0), "remainder-zero2");
checkExpect(remainder([NumV(-3), NumV(-2)]),NumV(-1),
  "remainder-negative",);
checkError(() => remainder([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => remainder([NumV(0)]), "Must operate on two integers.");
checkError(() => remainder([BoolV(true)]), "Must operate on two integers.");
checkError(() => remainder([]), "Must operate on two integers.");
checkError(() => remainder([ListV([NumV(3)])]),
"Must operate on two integers.",);

/* equal: list(value) -> value
   Input: The input of equal is a list of values, alod. equal should take in a
   list of two elements, both of which are NumV(int).
   Output: The output of equal is a BoolV(bool) containing a bool that
   signifies if the two elements in the input list are equivalent.
   BoolV(false) is produced if the elements are not equivalent, while
   BoolV(true) is produced if the elements are equivalent. If the input is
   not a list composed of only NumV(int) lists — the program will fail. */

let equal: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => BoolV(NumV(x) == NumV(y))
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(equal([NumV(3), NumV(-3)]), BoolV(false), "equal-false");
checkExpect(equal([NumV(-2), NumV(-2)]), BoolV(true), "equal-true");
checkExpect(equal([NumV(0), NumV(-3)]), BoolV(false), "equal-false2");
checkExpect(equal([NumV(0), NumV(0)]), BoolV(true), "equal-true2");
checkError(() => equal([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => equal([NumV(0)]), "Must operate on two integers.");
checkError(() => equal([BoolV(true)]), "Must operate on two integers.");
checkError(() => equal([]), "Must operate on two integers.");
checkError(() => equal([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* lessThan: list(value) -> value
   Input: The input of lessThan is a list of values, alod. lessThan should
   take in a list of two elements, both of which are NumV(int).
   Output: The output of lessThan is a BoolV(bool) indicating if the first
   element in the list is less than the second. BoolV(false) is produced if
   the first element is greater than or equal to the second. BoolV(true) is
   produced if the first element is less than the second. If the input is not
   a list composed of only NumV(int), the program will fail. */

let lessThan: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => BoolV(NumV(x) < NumV(y))
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(lessThan([NumV(3), NumV(2)]), BoolV(false), "lessthan-false");
checkExpect(lessThan([NumV(-3), NumV(2)]), BoolV(true), "lessthan-true");
checkExpect(lessThan([NumV(0), NumV(0)]), BoolV(false), "lessthan-false2",);
checkExpect(
  lessThan([NumV(-1), NumV(-1)]), BoolV(false), "lessthan-false3",);
checkExpect(lessThan([NumV(18), NumV(19)]),
  BoolV(true),"lessthan-true2",);
checkExpect(lessThan([NumV(19), NumV(19)]),
  BoolV(false), "lessthan-false4",);
checkError(() => lessThan([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => lessThan([NumV(0)]), "Must operate on two integers.");
checkError(() => lessThan([BoolV(true)]), "Must operate on two integers.");
checkError(() => lessThan([]), "Must operate on two integers.");
checkError(() => lessThan([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* greaterThan: list(value) -> value
   Input: The input of greaterThan is a list of values, alod. greaterThan
   should take in a list of two elements, both of which are NumV(int).
   Output: The output of greaterThan is a BoolV(bool) containing a bool that
   indicates if the first element in the list is greater than the second.
   BoolV(false) is produced if the first element is less than or equal to the
   second. BoolV(true) is produced if the first element is greater than the
   second. If the input is not a list composed of only NumV(int), the program
   will fail. */

let greaterThan: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => BoolV(NumV(x) > NumV(y))
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(greaterThan([NumV(3), NumV(2)]),
  BoolV(true), "greaterthan-true",);
checkExpect(greaterThan([NumV(-2), NumV(-3)]),
  BoolV(true), "greaterthan-true2",);
checkExpect(greaterThan([NumV(-3), NumV(-2)]),
  BoolV(false), "greaterthan-false1",);
checkExpect(greaterThan([NumV(-3), NumV(2)]),
  BoolV(false), "greaterthan-false2",);
checkExpect(greaterThan([NumV(0), NumV(0)]), BoolV(false),
"greaterthan-false3",);
checkExpect(greaterThan([NumV(-1), NumV(-1)]), BoolV(false),
"greaterthan-false4",);
checkExpect(greaterThan([NumV(18), NumV(19)]),
  BoolV(false), "greaterthan-false5",);
checkExpect(greaterThan([NumV(19), NumV(19)]),BoolV(false),
  "greaterthan-false6",);
checkError(() => multiply([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => greaterThan([NumV(0)]), "Must operate on two integers.");
checkError(() => greaterThan([BoolV(true)]),
"Must operate on two integers.",);
checkError(() => greaterThan([]), "Must operate on two integers.");
checkError(() => greaterThan([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* lessThanEqualTo: list(value) -> value
   Input: The input of lessThanEqualTo is a list of values, alod.
   lessThanEqualTo should take in a list of two elements, both of which are
   NumV(int). Output: The output of lessThanEqualTo is a BoolV(bool)
   indicating if the first element is less than or equal to the second.
   BoolV(true) is produced if this condition is met, while BoolV(false) is
   produced if this condition is unmet. If the input is not a list of only
   NumV(int), the program fails. */

let lessThanEqualTo: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => BoolV(NumV(x) <= NumV(y))
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(lessThanEqualTo([NumV(3), NumV(2)]),
  BoolV(false), "ltet-false",);
checkExpect(lessThanEqualTo([NumV(-2), NumV(-3)]),
  BoolV(false), "ltet.false2",);
checkExpect(lessThanEqualTo([NumV(-3), NumV(2)]),
  BoolV(true), "ltet-true",);
checkExpect(lessThanEqualTo([NumV(0), NumV(0)]),
  BoolV(true), "ltet.true2",);
checkExpect(lessThanEqualTo([NumV(-1), NumV(-1)]),
  BoolV(true), "ltet.true3",);
checkExpect(lessThanEqualTo([NumV(18), NumV(19)]),
  BoolV(true), "ltet.true2",);
checkExpect(lessThanEqualTo([NumV(19), NumV(19)]),
  BoolV(true), "ltet.true4",);
checkError(() => lessThanEqualTo([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => lessThanEqualTo([NumV(0)]),
  "Must operate on two integers.",);
checkError(() => lessThanEqualTo([BoolV(true)]),
  "Must operate on two integers.",);
checkError(() => lessThanEqualTo([]), "Must operate on two integers.");
checkError(() => lessThanEqualTo([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* greaterThanEqualTo: list(value) -> value
   Input: The input of greaterThanEqualTo is a list of values, alod.
   greaterThanEqualTo should take in a list of two elements, both of which are
   NumV(int).
   Output: The output of greaterThanEqualTo is a BoolV(bool) indicating if the
   first element is greater than or equal to the second. BoolV(true) is
   produced if this condition is met, while BoolV(false) is produced if this
   condition is unmet. If the input is not a list of only NumV(int), the
   program fails. */

let greaterThanEqualTo: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(x), NumV(y)] => BoolV(NumV(x) >= NumV(y))
    | _ => failwith("Must operate on two integers.")
    };

checkExpect(greaterThanEqualTo([NumV(3), NumV(2)]), 
BoolV(true), "gtet-true",);
checkExpect(greaterThanEqualTo([NumV(-2), NumV(-3)]), BoolV(true),
"gtet-true2",);
checkExpect(greaterThanEqualTo([NumV(-3), NumV(-2)]), BoolV(false),
  "gtet-false1",);
checkExpect(greaterThanEqualTo([NumV(-3), NumV(2)]),
  BoolV(false), "gtet-false2",);
checkExpect(greaterThanEqualTo([NumV(0), NumV(0)]),
  BoolV(true), "gtet-true3",);
checkExpect(
  greaterThanEqualTo([NumV(18), NumV(19)]),
  BoolV(false), "gtet-false4",);
checkError(() => greaterThanEqualTo([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two integers.",);
checkError(() => greaterThanEqualTo([NumV(0)]),
  "Must operate on two integers.",);
checkError(() => greaterThanEqualTo([BoolV(true)]),
  "Must operate on two integers.",);
checkError(() => greaterThanEqualTo([]), "Must operate on two integers.");
checkError(() => greaterThanEqualTo([ListV([NumV(3)])]),
  "Must operate on two integers.",);

/* isEqual: list(value) -> value
   Input: The input of isEqual is a list of values, alod. The list should
   contain two elements.
   Output: The output of isEqual is a BoolV(bool) indicating if the first
   element of the list is equal to the second. BoolV(true) is produced if this
   condition is met, while BoolV(false) is produced if this condition is
   unmet. If the input specification is not met, the program fails. */

let isEqual: list(value) => value =
  alod =>
    switch (alod) {
    | [BuiltinV(_), _] => BoolV(false)
    | [_, BuiltinV(_)] => BoolV(false)
    | [ClosureV(_), _] => BoolV(false)
    | [_, ClosureV(_)] => BoolV(false)
    | [hd, tl] => BoolV(hd == tl)
    | _ => failwith("Must operate on two values.")
    };

checkExpect(isEqual([NumV(0), NumV(0)]), BoolV(true), "isequal-true1");
checkExpect(isEqual([NumV(1), NumV(0)]), BoolV(false), "isequal-false");
checkExpect(isEqual([BoolV(true), BoolV(true)]),
  BoolV(true), "isequal-true2",);
checkExpect(isEqual([BoolV(false), BoolV(true)]),
  BoolV(false), "isequal-false2",);
checkExpect(isEqual([ListV([NumV(3)]), ListV([NumV(3)])]),
  BoolV(true), "isequal-true3",);
checkExpect(isEqual([ListV([NumV(0)]), ListV([NumV(-0)])]),
  BoolV(true), "isequal-true4",);
checkExpect(isEqual([ListV([NumV(0)]), ListV([NumV(3)])]),
  BoolV(false), "isequal-false3",);
checkError(() => isEqual([NumV(3), NumV(0), NumV(6)]),
  "Must operate on two values.",);
checkError(() => isEqual([NumV(0)]), "Must operate on two values.");
checkError(() => isEqual([BoolV(true)]), "Must operate on two values.");
checkError(() => isEqual([]), "Must operate on two values.");
checkError(() => isEqual([ListV([NumV(3)])]),
  "Must operate on two values.",);
checkExpect(isEqual([
ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),
    ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),]),
  BoolV(false), "isequal-false4",);
checkExpect(isEqual([
    ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),
    NumV(5),]),
  BoolV(false), "isequal-false5",);
checkExpect(isEqual([NumV(3),
ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),]),
  BoolV(false),"isequal-false6",);
checkExpect(isEqual([
    BuiltinV({bName: "<builtin-proc-+>", bProc: plus}),
    BuiltinV({bName: "<builtin-proc-+>", bProc: plus}),]),
  BoolV(false), "isequal-false7",);
checkExpect(
  isEqual([NumV(6), BuiltinV({bName: "<builtin-proc-+>", bProc: plus})]),
  BoolV(false), "isequal-false7",);
checkExpect(
  isEqual([BuiltinV({bName: "<builtin-proc-+>", bProc: plus}), NumV(6)]),
  BoolV(false), "isequal-false8",);

/* isNumber: list(value) -> value
   Input: The input of isNumber is a list of values, alod. The input list
   should contain a single element.
   Output: The output of isNumber is a BoolV(bool) indicating if the element
   of the input list was a NumV(int). BoolV(true) is produced if this
   condition is met, while BoolV(false) is produced if this condition is
   unmet. If the input specification is not met, the program fails. */

let isNumber: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(_)] => BoolV(true)
    | [BoolV(_)] => BoolV(false)
    | [ListV([_])] => BoolV(false)
    | [BuiltinV(_)] => BoolV(false)
    | [ClosureV(_)] => BoolV(false)
    | _ => failwith("Must operate on a single element.")
    };

checkExpect(isNumber([NumV(0)]), BoolV(true), "isnumber-true");
checkExpect(isNumber([BoolV(true)]), BoolV(false), "isnumber-false");
checkExpect(isNumber([ListV([NumV(3)])]), BoolV(false),
  "isnumber-false3",);
checkError(
  () => isNumber([NumV(3), NumV(0), NumV(6)]),
  "Must operate on a single element.",);
checkError(
  () => isNumber([BoolV(true), BoolV(false)]),
  "Must operate on a single element.",);
checkError(() => isNumber([]), "Must operate on a single element.");
checkError(
  () => isNumber([ListV([NumV(3)]), BoolV(true), BoolV(false)]),
  "Must operate on a single element.",);
checkExpect(
  isNumber([BuiltinV({bName: "<builtin-proc-+>", bProc: plus})]),
  BoolV(false),
  "isnumber-false4",);
checkExpect(
  isNumber([
    ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),]),
  BoolV(false), "isnumber-false5",);

/* isZero: list(value) -> value
   Input: The input of isZero is a list of values, alod. The input list
   should contain a single element, and contain only NumV(int).
   Output: The output of isZero is a BoolV(bool) indicating if the input
   element contains zero. BoolV(true) is produced if this condition is met,
   while BoolV(false) is produced if this condition is unmet. If the input
   specification is not met, the program fails. */

let isZero: list(value) => value =
  alod =>
    switch (alod) {
    | [NumV(0)] => BoolV(true)
    | [NumV(_)] => BoolV(false)
    | _ => failwith("Must operate on a number value.")
    };

checkExpect(isZero([NumV(0)]), BoolV(true), "iszero-true");
checkExpect(isZero([NumV(1)]), BoolV(false), "iszero-false");
checkExpect(isZero([NumV(-0)]), BoolV(true), "iszero-true2");
checkError(() => isZero([NumV(3), NumV(0), NumV(6)]),
  "Must operate on a number value.",);
checkError(() => isZero([BoolV(true)]), "Must operate on a number value.");
checkError(() => isZero([]), "Must operate on a number value.");
checkError(() => isZero([ListV([NumV(3)])]),
  "Must operate on a number value.",);

/* cons: list(value) -> value
   Input: The input of cons is a list of values, alod. The list of values
   should contain two elements. The first element should be a value, while the
   second element should be a ListV(list(value)).
   Output: The output of cons is a ListV(list(value)) containing the first
   element of the input list followed by the values contained within the
   original input ListV(list(value)). */

let cons: list(value) => value =
  alod =>
    switch (alod) {
    | [hd, ListV(tl)] => ListV([hd, ...tl])
    | _ =>
      failwith(
        "Must operate on one element and a nonempty input list, in order.",
      )
    };

checkExpect(cons([NumV(3), ListV([NumV(2), NumV(6)])]),
  ListV([NumV(3), NumV(2), NumV(6)]), "cons1",);
checkExpect(cons([BoolV(true), ListV([BoolV(false)])]),
  ListV([BoolV(true), BoolV(false)]), "cons2",);
checkExpect(cons([ListV([NumV(3), NumV(4)]), ListV([BoolV(false)])]),
  ListV([ListV([NumV(3), NumV(4)]), BoolV(false)]), "cons3",);
checkError(() => cons([NumV(3), NumV(0), NumV(6)]),
  "Must operate on one element and a nonempty input list, in order.",);
checkError(() => cons([]),
  "Must operate on one element and a nonempty input list, in order.",);
checkError(() => cons([ListV([BoolV(true)]), NumV(3)]),
  "Must operate on one element and a nonempty input list, in order.",);
checkError(() => cons([ListV([BoolV(true)]), NumV(3), NumV(2)]),
  "Must operate on one element and a nonempty input list, in order.",);
checkError(() => cons([ListV([NumV(3), NumV(4)]), NumV(2)]),
  "Must operate on one element and a nonempty input list, in order.",);
checkExpect(
cons([ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),
ListV([BoolV(false)]),]),
ListV([ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),
BoolV(false),]), "cons4",);
checkExpect(cons([BuiltinV({bName: "<builtin-proc-+>", bProc: plus}),
ListV([BoolV(false)]),]),
ListV([BuiltinV({bName: "<builtin-proc-+>", bProc: plus}), BoolV(false)]),
"cons5",);

/* first: list(value) -> value
   Input: The input of first is a list of values, alod. The list of values
   should contain a single element, a ListV(list(value)). The ListV should
   contain at least one element.
   Output: The output of first is the first element contained within the input
   ListV(list(value)). If the input specification is not met, the program will
   fail. */

let first: list(value) => value =
  alod =>
    switch (alod) {
    | [ListV([hd, ..._])] => hd
    | _ => failwith("Must operate on a nonempty list.")
    };

checkExpect(first([ListV([NumV(2), NumV(18), NumV(36)])]),
NumV(2), "first1",);
checkExpect(first([ListV([BoolV(false)])]), BoolV(false), "first2");
checkExpect(first([ListV([ListV([NumV(9)])])]),
ListV([NumV(9)]), "first3",);
checkError(() => first([BoolV(true)]), "Must operate on a nonempty list.");
checkError(() => first([]), "Must operate on a nonempty list.");
checkError(() => first([ListV([])]), "Must operate on a nonempty list.");
checkExpect(first([ListV([
ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),
BoolV(false),]),]),
  ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}), "first4",);
checkExpect(first([ListV([BuiltinV({bName: "<builtin-proc-+>", bProc: plus}),
BoolV(false),]),]),
  BuiltinV({bName: "<builtin-proc-+>", bProc: plus}), "first5",);

/* rest: list(value) -> value
   Input: The input of rest is a list of values, alod. The list of values
   should contain a single element, a ListV(list(value)). The ListV should
   contain at least one element.
   Output: The output of rest is a ListV(list(value)) containing every element
   of the input ListV with the exception of the first element. If the input
   specification is not met, the program will fail. */

let rest: list(value) => value =
  alod =>
    switch (alod) {
    | [ListV([_, ...tl])] => ListV(tl)
    | _ => failwith("Must operate on only a nonempty list.")
    };

checkExpect(rest([ListV([NumV(2), NumV(18), NumV(36)])]),
  ListV([NumV(18), NumV(36)]), "rest1",);
checkExpect(rest([ListV([BoolV(false)])]), ListV([]), "rest2");
checkExpect(rest([ListV([NumV(3), NumV(4)])]),
  ListV([NumV(4)]), "rest3",);
checkExpect(rest([ListV([NumV(1)])]), ListV([]), "rest4");
checkError(
  () => rest([BoolV(true)]),
  "Must operate on only a nonempty list.",);
checkError(() => rest([]), "Must operate on only a nonempty list.");
checkError(
  () => rest([ListV([])]),
  "Must operate on only a nonempty list.",);
checkExpect(rest([ListV([
      ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),
      BoolV(false),]),]),
  ListV([BoolV(false)]), "rest5",);
checkExpect(rest([ListV([BoolV(false),
BuiltinV({bName: "<builtin-proc-+>", bProc: plus}),]),]),
ListV([BuiltinV({bName: "<builtin-proc-+>", bProc: plus})]), "rest6",);

/* isEmpty: list(value) -> value
   Input: The input of isEmpty is a list of values, alod. The list of values
   should contain a single element.
   Output: The output of isEmpty is a BoolV(bool) indicating whether the input
   ListV is empty. BoolV(true) will be returned for an empty list. Other
   values, including a nonempty list, should return false. If the input
   specification is not met, the program will fail. */

let isEmpty: list(value) => value =
  alod =>
    switch (alod) {
    | [ListV([])] => BoolV(true)
    | [NumV(_)] => BoolV(false)
    | [BoolV(_)] => BoolV(false)
    | [ListV([_])] => BoolV(false)
    | [BuiltinV(_)] => BoolV(false)
    | [ClosureV(_)] => BoolV(false)
    | _ => failwith("Must operate on a single element.")
    };

checkExpect(isEmpty([ListV([])]), BoolV(true), "isempty-true");
checkExpect(isEmpty([NumV(3)]), BoolV(false), "isempty-false");
checkExpect(isEmpty([BoolV(true)]), BoolV(false), "isempty-false2");
checkExpect(isEmpty([ListV([NumV(2)])]), BoolV(false), "isempty-false4");
checkError(() => isEmpty([ListV([]), NumV(8)]),
  "Must operate on a single element.",);
checkError(() => isEmpty([]), "Must operate on a single element.");
checkError(() => isEmpty([ListV([NumV(3), NumV(5)])]),
  "Must operate on a single element.",);
checkExpect(isEmpty([
    ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),]),
  BoolV(false), "isempty-false5",);
checkExpect(isEmpty([BuiltinV({bName: "<builtin-proc-+>", bProc: plus})]),
BoolV(false), "isempty-false6",);

/* isCons: list(value) -> value
   Input: The input of isCons is a list of values, alod. The list of values
   should contain a single element.
   Output: The output of isCons is a BoolV(bool) indicating whether the input
   is [ListV(list(value))]. If this condition is met, BoolV(true) is produced.
   Otherwise, BoolV(false) should be produced. If the input specification is
   not met, the program will fail. */

let isCons: list(value) => value =
  alod =>
    switch (alod) {
    | [ListV(_)] => BoolV(true)
    | [NumV(_)] => BoolV(false)
    | [BoolV(_)] => BoolV(false)
    | [BuiltinV(_)] => BoolV(false)
    | [ClosureV(_)] => BoolV(false)
    | _ => failwith("Must only have one input element.")
    };

checkExpect(isCons([ListV([NumV(3)])]), BoolV(true), "iscons");
checkExpect(isCons([ListV([NumV(3), NumV(2)])]), BoolV(true), "iscons2");
checkExpect(isCons([BoolV(true)]), BoolV(false), "iscons-false");
checkExpect(isCons([NumV(3)]), BoolV(false), "iscons-false2");
checkError(() => isCons([BoolV(true), BoolV(false)]),
  "Must only have one input element.",);
checkError(() => isCons([NumV(0), NumV(18), NumV(9)]),
  "Must only have one input element.",);
checkExpect(isCons([
    ClosureV({cNameList: [Name("CS17")], cExpr: NumE(5), cEnv: []}),]),
  BoolV(false), "iscons-false3",);
checkExpect(isCons([BuiltinV({bName: "<builtin-proc-+>", bProc: plus})]),
  BoolV(false), "iscons-false4",);

/* isNot: list(value) -> value
   Input: The input of isNot is a list of values, alod. The input list should
   have a single element — a BoolV(bool).
   Output: The output of isNot is a BoolV(bool) containing the opposite of
   the input bool. [BoolV(true)] should produce BoolV(false). [BoolV(false)]
   should produce BoolV(true). The program should fail if the input spec is
   not met. */

let isNot: list(value) => value =
  alod =>
    switch (alod) {
    | [BoolV(n)] => BoolV(!n)
    | _ => failwith("Must take in only BoolV(bool).")
    };

checkExpect(isNot([BoolV(true)]), BoolV(false), "isnot");
checkExpect(isNot([BoolV(false)]), BoolV(true), "isnot2");
checkError(() => isNot([ListV([])]), "Must take in only BoolV(bool).");
checkError(
  () => isNot([BoolV(true), BoolV(false)]),
  "Must take in only BoolV(bool).",);
checkError(() => isNot([NumV(3)]), "Must take in only BoolV(bool).");

// The Initial Top-level Environment
let initialTle: environment = [
  (Name("+"), BuiltinV({bName: "<builtin-proc-+>", bProc: plus})),
  (Name("-"), BuiltinV({bName: "<builtin-proc-->", bProc: minus})),
  (Name("*"), BuiltinV({bName: "<builtin-proc-*>", bProc: multiply})),
  (Name("/"), BuiltinV({bName: "<builtin-proc-/>", bProc: divide})),
  (Name("remainder"),
  BuiltinV({bName: "<builtin-proc-remainder>", bProc: remainder}),),
  (Name("="), BuiltinV({bName: "<builtin-proc-=>", bProc: equal})),
  (Name("<"), BuiltinV({bName: "<builtin-proc-<>", bProc: lessThan})),
  (Name(">"), BuiltinV({bName: "<builtin-proc->>", bProc: greaterThan})),
  (Name("<="),
  BuiltinV({bName: "<builtin-proc-<=>", bProc: lessThanEqualTo}),),
  (Name(">="),
  BuiltinV({bName: "<builtin-proc->=>", bProc: greaterThanEqualTo}),),
  (Name(">="), BuiltinV({bName: "<builtin-proc-equal?>", bProc: isEqual})),
  (Name("isNumber"),
  BuiltinV({bName: "<builtin-proc-number?>", bProc: isNumber}),),
  (Name("isZero"), BuiltinV({bName: "<builtin-proc-zero?>", bProc: isZero}),),
  (Name("cons"), BuiltinV({bName: "<builtin-proc-cons>", bProc: cons})),
  (Name("first"), BuiltinV({bName: "<builtin-proc-first>", bProc: first})),
  (Name("rest"), BuiltinV({bName: "<builtin-proc-rest>", bProc: rest})),
  (Name("rest"), BuiltinV({bName: "<builtin-proc-empty?>", bProc: isEmpty}),),
  (Name("isCons"), BuiltinV({bName: "<builtin-proc-cons?>", bProc: isCons}),),
  (Name("isNot"), BuiltinV({bName: "<builtin-proc-not?>", bProc: isNot})),
];

//============================================================================
//============================================================================

/* lookup: (environment, name) -> option, value
    Input: The inputs of lookup are an environment and a name, env and n.
    Output: The output of lookup is an option(value) containing the value
    that corresponds to the name in the environment. For example, if the
    environment contains the binding (Name("CS"), SymbolC("17")), entering
    Name("CS") returns Some(SymbolC("17")).

   RECURSION DIAGRAM A
   OI:
   RI:
   RO:
   OO:

   RECURSION DIAGRAM B
   OI:
   RI:
   RO:
   OO: */

let rec lookup: (environment, name) => option(value) =
  (env, n) =>
    switch (env) {
    | [] => None
    | [(name, value), ...tl] =>
      if (name == n) {
        Some(value);
      } else {
        lookup(tl, n);
      }
    };

checkExpect(lookup([], Name("hi")), None, "lookup-ce1");
checkExpect(lookup([(Name("hi"), NumV(18)), (Name("CS"), NumV(17))],
Name("CS"),), Some(NumV(17)), "lookup-ce2",);
checkExpect(lookup(
[(Name("george"), BoolV(true)), (Name("CS"), NumV(17))], Name("george"),),
  Some(BoolV(true)), "lookup-ce3",);
checkExpect(
  lookup([(Name("hi"), ListV([NumV(3)]))], Name("hi")),
  Some(ListV([NumV(3)])), "lookup-ce4",);
checkExpect(lookup(
    [(Name("+"), BuiltinV({bName: "<builtin-proc-+>", bProc: plus})),
     (Name("-"), BuiltinV({bName: "<builtin-proc-->", bProc: minus})),
     (Name("*"), BuiltinV({bName: "<builtin-proc-*", bProc: multiply})),],
    Name("-"),),
  Some(BuiltinV({bName: "<builtin-proc-->", bProc: minus})), "lookup-ce5",);
checkExpect(lookup([(Name("c1"),
        ClosureV({cNameList: [Name("c01")], cExpr: NumE(3), cEnv: []}),),
      (Name("c2"),
        ClosureV({cNameList: [Name("c02")], cExpr: BoolE(true), cEnv: []}),),
      (Name("c3"),
        ClosureV({cNameList: [Name("c03")], cExpr: EmptyE, cEnv: []}),),],
Name("c3"),),
Some(ClosureV({cNameList: [Name("c03")], cExpr: EmptyE, cEnv: []})),
"lookup-ce6",);

/* parseExpression: concreteProgramPiece -> expression
   Input: The input of parseExpression is a concreteProgram, cpp.
   Output: The output of parseExpression is an expression, the result of
   converting NumberC(int) to NumE(int) and switching on all of the possible
   input lists and symbols. Cond, let, lambda, and, or, and if are all
   evaluated through different processes, while lambda, let, and cond require
   nested helper functions. If the input specification is not met, the program
   fails.

       - lambdaHelper: concreteProgramPiece -> name
       - Input: The input of lambdaHelper is a concreteProgramPiece, c, which
       - should be of form SymbolC(string).
       - Output: The output of lambdaHelper is a Name(string) containing the
       - string originally contained in the input SymbolC(string).

       - letHelper: concreteProgramPiece -> letPair
       - Input: The input of letHelper is a concreteProgramPiece, c, which
       - should be of form ListC([SymbolC(string), concreteProgramPiece]).
       - Output: The output of letHelper is a letPair containing the string in
       - the input SymbolC(string) as a Name(string) and the result of
       - applying parseExpression to the second element of the input ListC
       - under pairExpr.

       - condHelper: concreteProgramPiece -> condData
       - Input: The input of condHelper is a concreteProgramPiece, c, which
       - should be of form ListC([concreteProgramPiece,
       - concreteProgramPiece]).
       - Output: The output of condHelper is of type condData, where the input
       - conditionExpr is the result of applying parseExpression to the first
       - element of ListC, and resultExpr is the result of applying
       - parseExpression to the second element of the input ListC.

     RECURSION DIAGRAM A
     OI:
     RI:
     RO:
     OO:

     RECURSION DIAGRAM B
     OI:
     RI:
     RO:
     OO: */

let rec parseExpression: concreteProgramPiece => expression =
  cpp =>
    switch (cpp) {
    | NumberC(x) => NumE(x)
    | ListC(l) =>
      switch (l) {
      | [SymbolC("cond"), ...t] => CondE(List.map(condHelper, t))
      | [SymbolC("let"), ListC(a), b] =>
        LetE({
          letPairs: List.map(letHelper, a),
          letBody: parseExpression(b),
        })
      | [SymbolC("lambda"), ListC(a), b] =>
        LambdaE({
          nameList: List.map(lambdaHelper, a),
          lambdaBody: parseExpression(b),
        })
      | [SymbolC("and"), a, b] =>
        AndE(parseExpression(a), parseExpression(b))
      | [SymbolC("or"), a, b] =>
        OrE(parseExpression(a), parseExpression(b))
      | [SymbolC("if"), a, b, c] =>
        IfE({
          boolExpr: parseExpression(a),
          trueExpr: parseExpression(b),
          falseExpr: parseExpression(c),
        })
      | [SymbolC("if"), ..._] => failwith("Invalid procedure structure ")
      | [ListC(i), ...tl] =>
        ApplicationE(List.map(parseExpression, [ListC(i), ...tl]))
      | [SymbolC(s), ...tl] =>
        ApplicationE(
          List.append([NameE(Name(s))], List.map(parseExpression, tl)),
        )
      | _ => failwith("Expected function call after open parenthesis")
      }
    | SymbolC(s) =>
      switch (s) {
      | "true" => BoolE(true)
      | "false" => BoolE(false)
      | "empty" => EmptyE
      | _ => NameE(Name(s))
      }
    }
and lambdaHelper: concreteProgramPiece => name =
  c =>
    switch (c) {
    | SymbolC(n) => Name(n)
    | _ => failwith("Invalid lambda input!")
    }
and letHelper: concreteProgramPiece => letPair =
  c =>
    switch (c) {
    | ListC([SymbolC(n), a]) => {
        pairName: Name(n),
        pairExpr: parseExpression(a),
      }
    | _ => failwith("Invalid let input!")
    }
and condHelper: concreteProgramPiece => condData =
  c =>
    switch (c) {
    | ListC([hd, tl]) => {
        conditionExpr: parseExpression(hd),
        resultExpr: parseExpression(tl),
      }
    | _ => failwith("Invalid cond input!")
    };

checkExpectExpression(parseExpression(NumberC(0)), NumE(0), "parseE1");
checkExpectExpression(parseExpression(SymbolC("true")),
  BoolE(true), "parseE2",);
checkExpectExpression(parseExpression(SymbolC("false")),
  BoolE(false), "parseE3",);
checkExpectExpression(parseExpression(SymbolC("empty")), EmptyE, "parseE4");
checkExpectExpression(parseExpression(SymbolC("+")),
  NameE(Name("+")), "parseE5",);
checkExpectExpression(parseExpression(SymbolC("hello")),
  NameE(Name("hello")), "parseE6",);
checkExpectExpression(
parseExpression(ListC([SymbolC("+"), NumberC(3), NumberC(-5)])),
  ApplicationE([NameE(Name("+")), NumE(3), NumE(-5)]), "parseE7",);
checkExpectExpression(
  parseExpression(ListC([SymbolC("cons"), SymbolC("CS"), SymbolC("17")])),
  ApplicationE([NameE(Name("cons")), NameE(Name("CS")),
    NameE(Name("17")),]), "parseE8",);
checkExpectExpression(parseExpression(ListC([SymbolC("cond"),
ListC([ListC([SymbolC("empty?"), SymbolC("alod")]),
ListC([SymbolC("+"), NumberC(1), SymbolC("alod")])])])),
CondE([{conditionExpr: ApplicationE([NameE(Name("empty?")), 
NameE(Name("alod"))]),
resultExpr: ApplicationE([NameE(Name("+")), NumE(1), 
NameE(Name("alod"))])}]), "parseE9");
checkExpectExpression(
  parseExpression(ListC([ListC([SymbolC("lambda"), ListC([SymbolC("x"), 
  SymbolC("y")]), ListC([ListC([SymbolC("lambda"), ListC([SymbolC("y")]),
  ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")])]),
  SymbolC("x")])]),
  NumberC(17), NumberC(18)])), ApplicationE([LambdaE({nameList: [Name("x"), 
  Name("y")], lambdaBody: ApplicationE([LambdaE({nameList: [Name("y")],
  lambdaBody: ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
  NameE((Name("y")))])}), NameE((Name("x")))])}),
  NumE(17), NumE(18)]), "parseE10");
checkExpectExpression(
  parseExpression(ListC([SymbolC("let"), ListC([ListC([SymbolC("x"), 
NumberC(19)])]), ListC([SymbolC("+"), SymbolC("x"), NumberC(32)])])), 
LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(19)}], letBody: 
ApplicationE([NameE((Name("+"))), NameE((Name("x"))), NumE(32)])}), 
"parseE11");
checkExpectExpression(parseExpression(
ListC([SymbolC("and"), ListC([SymbolC(">"), NumberC(1), NumberC(4)]),
       ListC([SymbolC("<"), NumberC(2), NumberC(4)])])), 
       AndE(ApplicationE([NameE((Name(">"))), NumE(1), NumE(4)]),
 ApplicationE([NameE((Name("<"))), NumE(2), NumE(4)])), "andCheck");
checkError(() => parseExpression(ListC([])),
   "Expected function call after open parenthesis");
   checkError(() => parseExpression(ListC([SymbolC("let"),
   ListC([NumberC(3)]), NumberC(4), SymbolC("hello")])),
   "Expected function call after open parenthesis");
   checkError(() => parseExpression(ListC([SymbolC("let"), ListC([NumberC(3),
   NumberC(4), SymbolC("define")])])), 
   "Expected function call after open parenthesis");
   checkExpectExpression(parseExpression(
ListC([SymbolC("or"), ListC([SymbolC(">"), NumberC(1), NumberC(4)]),
       ListC([SymbolC("<"), NumberC(2), NumberC(4)])])), 
       OrE(ApplicationE([NameE((Name(">"))), NumE(1), NumE(4)]),
 ApplicationE([NameE((Name("<"))), NumE(2), NumE(4)])), "orCheck");
 checkExpectExpression(parseExpression(
ListC([SymbolC("if"), ListC([SymbolC(">"), NumberC(3), NumberC(4)]),
       NumberC(9), NumberC(10)])
), IfE({boolExpr: ApplicationE([NameE((Name(">"))), NumE(3), NumE(4)]),
     trueExpr: NumE(9), falseExpr: NumE(10)}), "Iftest2");
     checkExpectExpression(parseExpression(
ListC([SymbolC("if"), ListC([SymbolC("<="), NumberC(3), NumberC(100)]),
       NumberC(9), NumberC(10)])),
IfE({boolExpr: ApplicationE([NameE((Name("<="))), NumE(3), NumE(100)]),
     trueExpr: NumE(9), falseExpr: NumE(10)}), "Iftest2");

/* parseDefinition: concreteProgramPiece -> definition
   Input: The input of parseDefinition is a concreteProgramPiece, cpp. cpp
   should be of form ListC([SymbolC("define"), SymbolC(string),
   concreteProgramPiece]).
   Output: The output of parseDefinition is a definition, of form
   (Name(string)), parseExpression(concreteProgramPiece)), where the string
   is the string found in the input SymbolC(string) and concreteProgramPiece
   is the input concreteProgramPiece (the third element of the ListC). If the
   input specification is not met, the program fails. */

let parseDefinition: concreteProgramPiece => definition =
  cpp =>
    switch (cpp) {
    | ListC([SymbolC("define"), SymbolC(x), tl]) => (
        Name(x),
        parseExpression(tl),
      )
    | _ => failwith("Should only take in an input of the above form!")
    };

checkError(() => parseDefinition(SymbolC("define")),
  "Should only take in an input of the above form!",);
checkError(() => parseDefinition(ListC([])),
  "Should only take in an input of the above form!",);
checkError(() => parseDefinition(
      ListC([NumberC(3), SymbolC("define"), SymbolC("let"), NumberC(9)]),),
  "Should only take in an input of the above form!",);
checkError(() => parseDefinition(ListC([SymbolC("define")])),
  "Should only take in an input of the above form!",);
checkError(() => parseDefinition(ListC([SymbolC("define")])),
  "Should only take in an input of the above form!",);

/* parsePiece: concreteProgramPiece -> abstractProgramPiece
   Input: The input of parsePiece is a concreteProgramPiece, input.
   Output: The output of parsePiece is an abstractProgramPiece of form
   Definition(parseDefinition(input)) if the input was of form
   ListC([SymbolC("define"), ..._]). This is the result of applying
   parseDefinition to the input and wrapping it as a definition. If the input
   is of any other form, the output is of form Expression(parseExpression
   (input)). This is the result of applying parseExpression on the input list
   and wrapping it as an expresion. */

let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* NO TESTING FOR PARSEPIECE: This was a given function. */

/* parse: concreteProgram -> abstractProgram
   Input: The input of parse is a concreteProgram, input.
   Output: The output of parse is the result of mapping the procedure
   parsePiece onto the input concreteProgram. */

let parse: concreteProgram => abstractProgram =
  input => List.map(parsePiece, input);

checkExpectAbstractProgram(parse([NumberC(10)]), 
[Expression(NumE(10))], "parse1",);
checkExpectAbstractProgram(parse([NumberC(0)]),
  [Expression(NumE(0))], "parse2",);
checkExpectAbstractProgram(parse([]), [], "parse3");
checkExpectAbstractProgram(
  parse([ListC([SymbolC("+"), NumberC(1), NumberC(41)])]),
  [Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(41)]))],
  "parse4",);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("+"), NumberC(1), NumberC(41)])]),
  [Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(41)]))],
  "parse5",);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("if"),
      ListC([SymbolC(">"), NumberC(3), NumberC(2)]), SymbolC("hey boy"),
SymbolC("hey"),]),]),
  [Expression(IfE({
        boolExpr: ApplicationE([NameE(Name(">")), NumE(3), NumE(2)]),
        trueExpr: NameE(Name("hey boy")),
        falseExpr: NameE(Name("hey")),}),),], "parse6",);
checkExpectAbstractProgram(parse([ListC([
SymbolC("and"), ListC([SymbolC(">"), NumberC(3), NumberC(2)]),
ListC([SymbolC(">"), NumberC(9), NumberC(10)]),]),]),
  [Expression(AndE(
        ApplicationE([NameE(Name(">")), NumE(3), NumE(2)]),
        ApplicationE([NameE(Name(">")), NumE(9), NumE(10)]),),),], "parse7",);
checkExpectAbstractProgram(
  parse([ListC([SymbolC("define"), SymbolC("x"), NumberC(9)])]),
  [Definition((Name("x"), NumE(9)))], "parse8",);
checkExpectAbstractProgram(parse([ListC([SymbolC("define"),
SymbolC("f"), ListC([SymbolC("lambda"), ListC([SymbolC("x")]),
ListC([SymbolC("+"), SymbolC("x"), NumberC(4)]),]),]),]),
  [Definition((Name("f"), LambdaE({nameList: [Name("x")],
        lambdaBody: 
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(4)]),}),)),],
"parse9",);
checkExpectAbstractProgram(parse([NumberC(10), NumberC(9)]),
[Expression(NumE(10)), Expression(NumE(9))], "parse10",);
checkExpectAbstractProgram(parse([
    SymbolC("define"),
    ListC([SymbolC("set-equal?"), SymbolC("set1"), SymbolC("set2")]),
    ListC([
      SymbolC("and"),
      ListC([SymbolC("subset?"), SymbolC("set1"), SymbolC("set2")]),
      ListC([SymbolC("subset?"), SymbolC("set2"), SymbolC("set1")]),]),]),
  [Expression(NameE(Name("define"))),
    Expression(ApplicationE([NameE(Name("set-equal?")), NameE(Name("set1")),
    NameE(Name("set2")),]),),
    Expression(AndE(ApplicationE([NameE(Name("subset?")), NameE(Name("set1")),
    NameE(Name("set2")),]), ApplicationE([ NameE(Name("subset?")), 
    NameE(Name("set2")), NameE(Name("set1")),]),),),], "parse11",);
checkExpectAbstractProgram(parse([ListC([ListC([SymbolC("and"),
        ListC([SymbolC("cons?"), SymbolC("small-set")]),
        ListC([SymbolC("empty?"), SymbolC("big-set")]),]),
      SymbolC("false"),]),]),
  [Expression(ApplicationE([AndE(
    ApplicationE([NameE(Name("cons?")), NameE(Name("small-set"))]),
    ApplicationE([NameE(Name("empty?")), NameE(Name("big-set"))]),),
    BoolE(false),]),),],"parse12",);

/* LIMITED TESTING FOR PARSE: This was a given function. */

//============================================================================
//============================================================================

/* eval: (environment, environment, expression) -> value
   Input: The inputs of eval are two environments and an expression, tle, env,
   and expr, respectively.
   Output: The output of eval is the result of converting NumE(int) to
   NumV(int), converting BoolE(bool) to BoolV(bool), and evaluating the
   information within AndE(expression, expression), OrE(expression,
   expression), IfE(ifData), CondE(list(condData)), LambdaE(lambdaData),
   LetE(letData), and ApplicationE(list(expression)) according to independent
   processes. The output values are all of type value.

       - mapEval: (list(expression), environment, environment) -> list(value)
       - Input: The inputs of map are an expression list and two environments,
       - l, tle, and env respectively.
       - Output: The output of mapEval is the result of applying eval to each
       - element in the input list of expressions over the two environments.

       - bindMap: (list(name), list(value)) -> environment
       - Input: The inputs of bindMap are a list of names, nList, and a list
       - of values, eList.
       - Output: The output of bindMap is an environment where the first
       - element of the nList and the first element of the eList are entered
       - as a binding. The second elements are also entered as a binding. This
       - is recursive; it will repeat this process until it reaches the last
       - elements of either list. If the number of elements is not equal,
       - the program fails.

     RECURSION DIAGRAM A
     OI:
     RI:
     RO:
     OO:

     RECURSION DIAGRAM B
     OI:
     RI:
     RO:
     OO: */

let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    switch (expr) {
    | EmptyE => ListV([])
    | NumE(n) => NumV(n)
    | BoolE(b) => BoolV(b)
    | NameE(n) =>
      switch (lookup(List.append(env, tle), n)) {
      | Some(v) => v
      | None => failwith("Binding does not exist!")
      }
    | AndE(a, b) =>
      switch (eval(tle, env, a)) {
      | BoolV(false) => BoolV(false)
      | BoolV(true) =>
        switch (eval(tle, env, b)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("Must evaluate to boolean!")
        }
      | _ => failwith("Must evaluate to boolean!")
      }
    | OrE(a, b) =>
      switch (eval(tle, env, a)) {
      | BoolV(true) => BoolV(true)
      | BoolV(false) =>
        switch (eval(tle, env, b)) {
        | BoolV(false) => BoolV(false)
        | BoolV(true) => BoolV(true)
        | _ => failwith("Must evaluate to boolean!")
        }
      | _ => failwith("Must evaluate to boolean!")
      }
    | IfE({boolExpr: a, trueExpr: b, falseExpr: c}) =>
      switch (eval(tle, env, a)) {
      | BoolV(true) => eval(tle, env, b)
      | BoolV(false) => eval(tle, env, c)
      | _ => failwith("Must evaluate to a boolean!")
      }
    | CondE([{conditionExpr: a, resultExpr: b}, ...tl]) =>
      switch (eval(tle, env, a)) {
      | BoolV(true) => eval(tle, env, b)
      | BoolV(false) => eval(tle, env, CondE(tl))
      | _ => failwith("Condition must evaluate to a boolean")
      }
    | CondE([]) => failwith("None of the conditionals were true")
    | LetE({letPairs: list, letBody: b}) =>
      switch (list) {
      | [] => eval(tle, env, b)
      | [{pairName: n, pairExpr: e}, ...tl] =>
        eval(
          tle,
          [(n, eval(tle, env, e)), ...env],
          LetE({letPairs: tl, letBody: b}),
        )
      }
    | LambdaE(d) =>
      switch (d) {
      | {nameList: l, lambdaBody: b} =>
        ClosureV({cNameList: l, cExpr: b, cEnv: env})
      }
    | ApplicationE(lst) =>
      let newList = mapEval(lst, tle, env);
      switch (List.hd(newList)) {
      | BuiltinV({bName: _, bProc: p}) => p(List.tl(newList))
      | ClosureV({cNameList: l, cExpr: b, cEnv: e}) =>
        eval(tle, List.append(bindMap(l, List.tl(newList)), e), b)
      | _ => failwith("Invalid Input")
      };
    }
and mapEval: (list(expression), environment, environment) => list(value) =
  (l, tle, env) =>
    switch (l) {
    | [] => []
    | [hd, ...tl] => [eval(tle, env, hd), ...mapEval(tl, tle, env)]
    }
and bindMap: (list(name), list(value)) => environment =
  (nList, eList) =>
    switch (List.length(nList)) {
    | n when n == List.length(eList) =>
      switch (nList) {
      | [] => []
      | [hd, ...t] => [
          (hd, List.hd(eList)),
          ...bindMap(t, List.tl(eList)),
        ]
      }
    | _ =>
      failwith("Number of formal arguments does not match number of
   actuals!",
      )
    };

//============================================================================
//============================================================================

   /* addDefinition: (environment, (name, expression)) -> environment
       Input: The inputs of addDefinition are an environment and a name,
       expression pair, env, (id, expr) respectively.
       Output: The output of addDefinition is a new environemnt that is
       unchanged other than containing the given (name, expression) pair as
       another element in the list. 
       
       RECURSION DIAGRAM A
       OI:
       RI:
       RO:
       OO:

       RECURSION DIAGRAM B
       OI:
       RI:
       RO:
       OO: */

let rec addDefinition: (environment, (name, expression)) => environment =
   (env, (Name(s), expr)) => switch(env) {
     | [] => [(Name(s), eval(initialTle, [], expr))]
     | [(Name(n), _),..._] when n == s => failwith("Already defined")
     | [(Name(n), _),...tl] when n != s => addDefinition(tl, (Name(s), expr))
     | _ => failwith("invalid")
     };


   /* listIt: list(string) -> string
      Input: The input of listIt is a string list, l.
      Output: The output of listIt is the string contained inside the list if
      there is only one element, or if the list contains multiple elements it is
      the first element and the result of applying listIt to the rest of the
      list of strings.

   RECURSION DIAGRAM A
   OI:
   RI:
   RO:
   OO:

   RECURSION DIAGRAM B
   OI:
   RI:
   RO:
   OO: */

  let rec listIt: list(string) => string = l =>
    switch(l) {
    | [] => "[]"
    | [hd] => hd
    | [hd, ...tl] => hd ++" " ++ listIt(tl)
  }

   /* stringOfValue: value -> string
       Input: The input of stringOfValue is a value, aValue. This represents a
       read, parsed, and processed Racket code.
       Output: The output of stringOfValue is a string containing all the
       internal elements of the value. stringOfValue will essentially "pop"
       all the internal representations out of their wrappers and present the
       strings.

      - sovHelper: name -> string
      - Input: The input of sovHelper is a name, n.
      - Output: The output of sovHelper is the string found inside the name!

    RECURSION DIAGRAM A
    OI:
    RI:
    RO:
    OO:

    RECURSION DIAGRAM B
    OI:
    RI:
    RO:
    OO: */

let rec stringOfValue: value => string = 
  aValue => switch(aValue) {
    | NumV(i) => string_of_int(i)
    | BoolV(i) => string_of_bool(i)
    | ListV([]) => "empty"
    | ListV(i) => listIt(List.map(stringOfValue, i))
    | BuiltinV({bName: s, bProc: _}) => "builtin: " ++ s
    | ClosureV({cNameList:l, cExpr: _, cEnv: _}) => 
    "(lambda (" ++ listIt(List.map(sovHelper, l)) ++ ") ... ) "  
  }
and sovHelper: name => string 
= n => switch(n) {
  |Name(str) => str
}

//============================================================================
//============================================================================

   /* process: abstractProgram -> list(value)
      Input: The input of process is an abstractProgram, pieces.
      Output: The output of process is a list(value). If the input list was
      empty, the output will be an empty list ([]). Otherwise, definitions will
      be processed by utilizing recursion in processHelper and the helper
      addDefinition to add a definition to the environment. If the input is an
      expression, the helper eval will be called to lookup the expression in the
      environment, also using recursion in processHelper. This will return the
      "processed" abstractProgram as a list of values, and this list will
      contain the inputs of stringOfValue.

   processHelper RECURSION DIAGRAM A
   OI:
   RI:
   RO:
   OO:

   processHelper RECURSION DIAGRAM B
   OI:
   RI:
   RO:
   OO: */

   let process: abstractProgram => list(value) =
    pieces => {
      let rec processHelper: (environment, abstractProgram) => list(value) =
        (tle, pieces) =>
          switch (pieces) {
          | [] => []
          | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
          | [Expression(e), ...tl] => [
              eval(tle, [], e),
              ...processHelper(tle, tl),
            ]
          };
      processHelper(initialTle, pieces);
    };

  /* NO TESTING FOR PROCESS: This was a given function. */

   /* rackette: rawProgram -> list(string)
      Input: The input of rackette (!) is a rawProgram, program, representing a
      syntactically correct Racket program.
      Output: The output of rackette is a list of strings that is the result of
      mapping stringOfValue across the list of values that is the output of
      process, parse, and readAll. This will of course utilize every respective
      and neccesary helper function for each function. */

   let rackette: rawProgram => list(string) =
    program => List.map(stringOfValue, process(parse(readAll(program))));

checkExpect(rackette("(((lambda(u) (lambda (x) (+ x u))) 4) 7)"), ["11"], 
"BEAST");
checkExpect(rackette("(+ 3 2)"), ["5"], "rackette1");
checkExpect(rackette("(/ 3 2)"), ["1"], "rackette1.1")
checkExpect(rackette("(* 3 2)"), ["6"], "rackette1.2")
checkExpect(rackette("(- 3 2)"), ["1"], "rackette1.3")
checkExpect(rackette("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"),
["34"], "rackette2");
checkExpect(rackette("((lambda (x y) ((lambda (x) (+ x y)) x)) 17 18)"),
["35"], "rackette3");
checkExpect(rackette("(let ((x 0)) (let (( f (lambda (a) (* x a )))) 
(let ((x 1 )) (f 5))))"), ["0"], "rackette4");
checkExpect(rackette("(let ((x 0) (y 18)) (let ((f (lambda (a b) 
(+ x b ))) (x 17)) (f y x)))"), ["17"], "rackette5");
checkExpect(rackette("(cons 1 (cons 2 empty))"), ["1 2"], "rackette5.5")
checkExpect(rackette("1"), ["1"], "rackette6");
checkExpect(rackette("(define fact (lambda (x) (if (zero? x) 1 (* x
 (fact ( - x 1)))))) (fact 3)"), ["3"], "rackette7");
 checkExpect(rackette("(define y 17) (let ((y 3)) (+ y 7))"), ["10"],
 "rackette8");

//============================================================================
//============================================================================


checkExpect(eval(initialTle, [], ApplicationE([NameE((Name("+"))), NumE(1), NumE(3)])), NumV(4), "evalCheck1")


checkExpect(eval(initialTle, [], ApplicationE([NameE((Name(">"))), NumE(1), NumE(3)])), BoolV(false)
, "evalCheck2")

checkExpect(eval(initialTle, [], IfE({
        boolExpr: ApplicationE([NameE(Name(">")), NumE(3), NumE(2)]),
        trueExpr: NumE(10),
        falseExpr: NumE(6)})), NumV(10)
, "evalCheck3")

checkExpect(eval(initialTle, [], OrE(ApplicationE([NameE((Name(">"))), NumE(1), NumE(4)]),
 ApplicationE([NameE((Name("<"))), NumE(2), NumE(4)]))
), BoolV(true)
, "evalCheck4")

checkExpect(eval(initialTle, [], AndE(ApplicationE([NameE((Name(">"))), NumE(1), NumE(4)]),
 ApplicationE([NameE((Name("<"))), NumE(2), NumE(4)]))
), BoolV(false), BoolV(false)
, "evalCheck5")



checkExpect(eval(initialTle, [], ApplicationE([LambdaE({nameList: [Name("x"), 
  Name("y")], lambdaBody: ApplicationE([LambdaE({nameList: [Name("y")],
  lambdaBody: ApplicationE([NameE((Name("+"))), NameE((Name("x"))),
  NameE((Name("y")))])}), NameE((Name("x")))])}),
  NumE(17), NumE(18)])), NumV(34)
, "evalCheck6")

checkExpect(eval(initialTle, [], LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(19)}], letBody: 
ApplicationE([NameE((Name("+"))), NameE((Name("x"))), NumE(32)])})), NumV(51)
, "evalCheck7")


checkExpect(eval(initialTle, [], IfE({boolExpr: ApplicationE([NameE((Name("<="))), NumE(3), NumE(100)]),
     trueExpr: NumE(9), falseExpr: NumE(10)})), NumV(9)
, "evalCheck8")

checkExpect(eval(initialTle, [], 
ApplicationE([NameE((Name("*"))), NumE(4),
              ApplicationE([NameE((Name("+"))), NumE(2),
                            ApplicationE([NameE((Name("-"))), NumE(3), NumE(4)])])])
), NumV(4)
, "evalCheck9")

checkExpect(eval(initialTle, [], NameE(Name("+"))),BuiltinV({bName: "<builtin-proc-+>", bProc: <fun>})
, "evalCheck10")

checkExpect(eval(initialTle, [],
LambdaE({nameList: [Name("x")],
         lambdaBody:
          ApplicationE([NameE((Name("+"))), NumE(1), NameE((Name("x")))])})
), 
ClosureV({cNameList: [Name("x")],
          cExpr:
           ApplicationE([NameE((Name("+"))), NumE(1), NameE((Name("x")))]),
          cEnv: []})

, "evalCheck11")

checkExpect(eval(initialTle, [], ApplicationE([NameE((Name("cons"))), NumE(1), EmptyE])
),ListV([NumV(1)])
, "evalCheck12")




checkExpect(stringOfValue(NumV(1)), "1", "sov1" )
checkExpect(stringOfValue(BoolV(false)), "false", "sov2" )
checkExpect(stringOfValue(BoolV(true)), "true", "sov3" )
checkExpect(stringOfValue(ListV([NumV(42), NumV(42), NumV(42)])), "42 42 42", "sov4" )
checkExpect(stringOfValue(BuiltinV({bName: "<builtin-proc-+>", bProc: plus})), "builtin: <builtin-proc-+>"
, "sov5" )
checkExpect(stringOfValue(
ClosureV({cNameList: [Name("x")],
          cExpr:
           ApplicationE([NameE((Name("+"))), NumE(1), NameE((Name("x")))]),
          cEnv: []})), "(lambda (x) ... ) "
, "sov6" )





