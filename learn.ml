(*

to run OCaml files start utop then type the directive  #use "<filename>.ml"

to rerun the file after any changes re run the above directive in a new utop session.

A functional language 
    (i) defines computations are mathematical functions 
    (ii)avoids a mutated state

state = information maintained by a computation 
mutability = ability to change

functional programming 
    (i) represents computations as mathematical functions 
    (ii) embraces immutability

mutability breaks referential transperency
referential transperency = ability to replace a expression with a value without the effecting the result of the computation

in functional programming 
    (i)functions aren't supposed to have "side effects" . side effects = the result of a computation where a function mutates something that is outside the function 
    (ii)variables never change value
    (iii) expressions specify computations i.e. they say what to do not how to do.

int x = 0 
int incr() {
    x++;
    return x 
}

"A language that doesn't affect the way you look at programming isn't worth knowing." - Alan J. Perils 

Garbage Collectors were first introduced by functional programming languages then some imperative langauges adopted it. (Lisp 1958)
LISP is the first functional programming language ?

Generics(Parametric Polymorphism) were first introduced by functional programming(ML (1990)) then adopted by imperative languages like Java

Higher Order Functions were first introduced by LISP then they were adopted by C# and Java

Autmatic type inference was first introduced by functional programming languages (ML 1990)

Pattern Matching was first introduced by functional programming languages then adopted by Rust

OCaml = Object Oriented (added later to this language) + Categorical Abstract Machine Language

OCaml is part of the ML family of languages. 

ML = meta language ? 

Aspects to learning a programming language 
(1)Syntax
(2)Semantics (Type Checking, Evaluation Rules)
(3)Idioms (typical patterns used to express computations in this language)
(4)Libraries (third party packages , std)
(5)Tools (tools which help you write program in this language Ex. compiler / interpreter; LSP, debugger, top level(REPL))

focus more on semantics and idioms

In functional programming the basic building block of a program is a expression. Expressions are equivalent to statements or commands in imperative languages 

every expression has two things:
    (1)Syntax 
    (2)Semantics
        (i)Type Checking Rules(Static Semantics) = static as program is not running yet and type checking happens and if everything is legal a type is produced
        (ii)Evaluation Rules = produces a value, a exception or a infinite loop

Value is a expression that doesn't need any further evaluation.

All values are expression but not all expressions are values

utop = universal top level for ocaml 

In OCAML all expressions should end with ;;

You should read the outputs of a expression right to left in utop

utop # 9 ;;
- : int = 9

9 is the value of type int 

booleans in ocaml are true and false

utop # 9 > 10;;
- : bool = false

strings are enclosed in "" 

utop # "Hello, World!";;
- : string = "Hello, World!"


stirng concatenation

utop # "Hello" ^ " , World";;
- : string = "Hello , World"

To multiply two ints use * 
utop # 3 * 4;;
- : int = 12

To multiply two floats use *.
utop # 2.0 *. 3.14;;
- : float = 6.28

This is a design choice in OCaml. Designers of OCaml decided not to overload operators 

OCaml has type inference
OCaml does type checking at compile time not run time

You can specifiy the types of a variable using type annotations using : and by putting paranthesis around the whole thing
e  => (e : t)
utop # (2 : int) * (3 : int);;
- : int = 6

if then else expressions
utop # if 2 > 3 then "Hello" else "World";;
- : string = "World"

strings are ordered in lexicographic order
utop # if "a" > "b" then "A" else "B";;
- : string = "B"

the condition after the if should be a bool not even 0
the below code will not compile
if 0 then "A" else "B";;

both the "then" and "else" branches should have expressions that evaluate to the same type

here if then else is like the ternary operator in C famliy of languages ( ? : )

if exp1 then exp2 else exp3

"defintion" lets us give a name to a value. in other languages this is called as variables.
only thing that is differnet here is variables are immutable by default.

defintion is done using the keyword let 

utop # let x : int = 9;;
val x : int = 9

reading the output from right to left the value is 9 its type is int and is bound to name x thats what val x means here

definitions are NOT expression and expressions are NOT defintions
but defintions can contain expressions 

in the above example the x is called the identifier.

All indentifiers should start with a lower case letter

Evaluation of a definition
    let x = e (* here e is a expression*)
    (*e evaluates to value v*)
    (*then bind v to x*)
    (*henceforth x will evaluate to v*)


let x = e is not a expression so you can't use this as a sub-expression in some expression
defintions by themselves don't have a value 

so the below line will result in a compile time error
(let x = 1) + 2;; 

let expressions  : where defintions return something
utop # let a = 1 in a + 3;;
- : int = 4

after this if try to evaluate "a" again it won't work 
utop # a;;
Error: Unbound value a

here a is alive only for the sub expression after "in"

utop # let a = 3 in (let b = 4 in a + b);;
- : int = 7

------------- MUTATION ? ---------------------
utop # let e = 5 in (let e = 6 in e);;
Line 1, characters 4-5:
Warning 26 [unused-var]: unused variable e.

- : int = 6
----------------------------------------------

let definitions without the in keyword mean the defintion is valid in all of below expression

let y = 7;; this means y = 7 in all expressions below

let a = 1;;                                 let a = 1 in
let b = 2;;   (Top Level understanding) =>  let b = 2 in
let c = a + b;;                             let c = a ^ b in

So any variable(identifier) evaluation is just a substitution
scope is where a name(identifier) is meaningful
let a = 1 in 
(*b is not alive here*)
(
        let b = 2 in
                a + b        
)
(*b is not alive here*)

so ,
let x = 1 in 
(
    let x = 2 in x
) + x

the above code evaluates to 3 because of the scoping and this is why its not a mutation
this type of code is not advisable as its hard to understand.

utop # let x = 1 in (let x = 2 in x) + x;;
- : int = 3

utop # let x = 1 in (let x = 2 in x);;
Line 1, characters 4-5:
Warning 26 [unused-var]: unused variable x.

- : int = 2

in the above example the outer x in not used at all hence the warning. in the example above the above example there the outer x is also used

                
let expressions can be useful to give a scope to a variable

let x = 1;;
let x = 2;;
print_int x;; (*evaluates to 2*)

the above block is valid ocaml as it evaluates to 
let x = 1 in ( 
    let x = 2 in ( (*different peice of memory is allcated to this x*)
        print_int x
    )
)

is the above thing a example of shadowing ? 

the most basic kind of function in OCaml is the anonymous function

utop # (fun x -> x + 1);;
- : int -> int = <fun>

use the paranthesis in cases where you need to make the parsing happen in ways you want it to happen

utop # fun x -> x + 1 1;;
Error: This expression has type int
       This is not a function; it cannot be applied.

utop # (fun x -> x + 1) 1;;
- : int = 2

a anonymous function to get the average of a two floating point numbers 
utop # (fun x y -> (x +. y) /. 2.) 3.0 4.0;;
- : float = 3.5

anonymous function : fun x1 x2 .... xn -> e

Evaluation of a function : 
    (1) A function is a value so no further computation is necessary
    (2) The expression e is not evaluated until the function is applied (lazy evaluation ?? )

Anonymous functions are also called as lambda expressions
lambda means what follows is an anonymous function

In OCaml functions are values i.e.
    (1)Functions can take functions as arguments 
    (2)Functions can return functions

For function application no paranthesis are required in OCaml unless you need to force a particular order of evaluation
For function application putting commas in between the arguments will result in a compile error

e0 e1 e2 .... en
Evaluation :
    (1) evaluate sub expression e0 => v0 .... en => vn
    (2) v0 must be a function fun x1, x2, .... xn -> e
    (3) Substiture x1 with v1 .... xn with vn in e resulting in a new expression e' 
    (4) Evaluate e' => v
    (5) Result is v
    
(fun x -> x + 1) (2 + 3)
(fun x -> x + 1) (5)
(5 + 1)
(6)
6

(fun x y -> x - y) (3 * 1) (3 - 1)
(fun x y -> x - y) (3) (2)
(3 - 2)
(1)
1

Named Functions
*)

let inc = fun x -> x + 1 ;;

(*another way to write the above function*)
let inc x = x + 1;;

(*for floats*)
let f_inc x = x +. 1. ;;

(*average function for int and returns a int i.e. there may be data loss*)
let avg x y = (x + y) / 2

(*for floats*)
let f_avg x y = (x +. y) /. 2.;;

(*
    (fun x -> x + 2) 1
    is equivalent to
    let x = 1 in x + 2 (*this is syntactic sugar*)
*)

f_avg 3. 4.;;

(*
let f x y = x - y in f 3 2
let f = fun x y -> x - y in f 3 2
(fun x y -> x - y) 3 2 
(3 - 2)
(1)
1

*)

(* If a function is recursive you must explicitly state that*)
let rec fibonacci n =
    match n with
    | 0 -> 0 
    | 1 -> 1
    | _ -> fibonacci(n - 1) + fibonacci(n - 2)

let rec factorial_if_then_else n = 
    if n <= 0 then 1 else n * factorial_if_then_else(n - 1);;

let rec factorial n = 
    match n with 
    | 0 -> 1
    | 1 -> 1
    | _ -> n * factorial(n - 1)

(*
    Type Checking of functions 
    functions of type t1 -> t2 take in values of type t1 and return values of type t2
    functions of type t1 -> t2 -> t3 take in two values one of type t1 and another of type t2 and return a value of type t3
    usually the last type is the return type and rest are types of input arguments
    -> is used to represent the type of a function and to also assign a function its value (fun x -> x + 1) ?? 

    If x1 , x2, x3 .... xn are of type t and e is of type u then the function fun x1, x2 ... xn -> e is of type t -> t -> t.... -> t -> u
*)

(*
    Partial Application
    lets say we have a function named 
    let add x y = x + y;; (* type = int -> int -> int *)
    add 2 3;; (* this will return 5*)
    add 2;; (* this will return a function of type int -> int *)
*)

let add x y = x + y;; 

add 2 3;;

add 2 ;; 

let add2 = add 2 ;; 

add2 3;;

(*
    In truth multi argument functions don't exist in OCaml 
    multi argument functions are syntactic sugar for single argument functions
    fun x y -> e ==> fun x -> (fun y -> e)
    this is why partial application works

    let add x y = x + y ==> let add = fun x -> (fun y -> x + y)
*)

(* Identity Function *)
let id x = x ;; 

(*
    id : 'a -> 'a
    'a is a type variable indicating the fact that x here cannot be inferred by the compiler 
    'a is called as alpha , 'b is called as beta and 'c is called as gamma
    this is a kind of polymorphism where x can be of any type
    polymorphic functions =  functions that work for many types of arguments
    very similar to Java's generics and some what similar to C++ templates
    this is called Parametric Polymorphism
    For every type this functions is called a new function for that specific type is created ??? 
*)

id "Hello" ;; 

id 2;;

(*
    Binary Operators as functions 
    (+) 1 2 this evaluates to 3
*)

(+) 1 2 ;; (* prefix notation *)

(* be careful to do the above thing for multiplication as OCaml may think its a comment*)
( * ) 3 4 ;; (* give space on both sides of * here to make sure it isn't parsed as a comment*)

( = ) 1 2 ;; (* ( = ) is a polymorphic comparison function *)

( = ) 2 2 ;; 

( = ) "Hello" "Hello" ;; 

( = ) "Hello" "hello" ;; 

(* ||'ly *)

( < ) 3  4 ;;

( > ) 3  4 ;;

(* we can define our own infix binary operators *)
max 3 4;; (* this max function is provided by the standard library *)

let ( ^? ) x y = max x y ;; (* the new operator has to be inside paranthesis otherwise this is a compile error*)
(* there are some restrictions are which symbol can be used for creating operators ??? *)

3 ^? 4 ;; (* evaluates to 4 *) 

succ 2 * 10 ;; (* succ is same increment function but is provided by the standard library *)
(* this evaluates to (succ 2) * 10 as function are more priority than binary operator *)
(* to succ (2 *10) application operator can be used*)

(* Application operator @@ *)
succ @@ 2 * 10 ;; (* @@ changes the precendence rules *)

let square n = n * n ;;
(* Reverse Application Operator also called as the Pipeline Operator |> *)
(* same thing as application operator but takes the arguments in a different order *)
(* to get the square of increment of 5*) 
square (succ 5) ;; 

(* the same thing can be written using pipeline operator *)
5 |> succ |> square ;; (* 5 |> succ |> square ==> 6 |> square ==> 6 * 6 ==> 36 *)

(* 
    Lists
        (1)Immutable 
        (2)Singly Linked List 
        (3)    
*)
[] ;; (* empty list of type 'a . nil list *)

[1; 2; 3] ;; (* int list *)

[1. ; 2. ; 3. ] ;; (* float list *)

[true; false; false] ;; (* bool list *)

(* list of lists *)
[[1; 2]; [3 ; 4]; [5; 6]] ;; 

(* to prepend a element to a list *)
1 :: [2 ; 3] ;; (* :: is called the cons operator*)

1 :: 2 :: 3 :: [] ;; 

[3; 4; 5] ;; (* this is syntactic sugar for  3 :: 4 :: 5 :: [] *)

(* 
    List Evaluation 
    [] is a value 
    To evaluate e1 :: e2 
        (1)evaluate e1 to value v1
        (2)evaluate e2 to a list value v2 
        (3)return v1 :: v2

    List Types = for any type t the type t list describes the list where all of its elements are of type t

    You can add a element of any type to a nil list

    Operator Precedence in OCaml ?? 

    Whenever a new element is prepended a new list is created as lists are immutable in OCaml

*)

(*
    Records. Similar to structs. Order of fields is irrelevant. 
    The max number of fields you can have in a struct is around 4 million
    Evaluation:
        Just evalutes the expressions to values 

    Record types must be defined before usage
*)

type vec = { 
    x : int; 
    y : int; 
} ;;

let v1 = { 
    x = 1;  
    y = 2;
} ;;

let v2 : vec = {
    x = 2;
    y = 4;
} ;;

v2.x ;;

(* to create a copy of a record  with some changes *)
let v4  = { v2 with x = 5 } ;; (* creates a new record not mutation. You cannot add new fields here as that would change the record type  *)

(* field names are identifiers not expressions *)

(* Tuples. Here order of elements is relevant unlike records as tuples are accessed by position
    {e1 , e2 , ... , en} => {v1, v2, v3, ..., vn}
    type of tuple is v1 * v2 * v3 .... * vn
*)
let tup1 = (9, 9, "Hello") ;; (* tuple of type int * int * string *)

type numTup = int * int * int * float ;; (* * here means cartesian product ??? *)

let tup2 = (1, 2, 3, 3.4) ;; 

let tup3 : numTup = (1, 2, 3, 3.4) ;; 

type vec2dtup = int * int

let v3 = (3 , 4) ;; 

(* to get the first element of any tuple *)
(* both fst and snd only work on tuples with only two elements ?? *)
fst ;; 

fst v3 ;; 

snd ;; 

snd v3 ;;

(*
    Pattern Matching 
    First successfull match is executed 
    If no patterns are matching then a Match_Failure exception is raised

    match n in
        p1 -> e1 (* one branch *)
        p2 -> e2
            .
            .
            .
            .
        pn -> en 

    all patterns(p1, p2, p3 .... pn) should have the same type and all expressions( e1, e2, ... , en) should evaluate to same type
    the type of the entire match expression is the type of the expression

    Patterns can be 
        (1) constants (matches itself)
        (2) identifiers (mathces anything and binds it to the scope of the branch)
        (3) _ (its called the wildcard it matches everything but doesn't bind it )

    In pattern matching the patterns should be exhaustive i.e. they should cover all cases if not the compiler will give you a warning
    the compiler will give you a warning if some pattern will never be matched.

    All of the above happens during static semantics
*)
let even_or_odd num = 
    match num mod 2 with 
    | 0 -> print_string "even"
    | _ -> print_string "odd";; (* _ indicates all the remaining cases *)

even_or_odd 2 ;;
even_or_odd 3 ;;

let first_element_of_a_string list_of_strings = 
    match list_of_strings with 
    | [] -> "empty"
    | h :: t -> h ;;
    
first_element_of_a_string ["Hello" ; "World"] ;; 

let get_list_without_the_first_element list_of_strings = 
    match  list_of_strings with
    | [] -> ["empty"]
    | h :: t -> t;;

get_list_without_the_first_element ["Hello" ; "World"; "Galaxy"] ;; 

let fst3 triplet = 
    match triplet with 
    | (a, b, c) -> a;;

fst3 ("Hello" , "World", "Galaxy") ;; 

let pretty_print_vec v = 
    match v with 
    | {x ; y} -> "2D vector : ( " ^ string_of_int(x) ^ " , " ^ string_of_int(y) ^ " )" ;;

pretty_print_vec v2 ;;

(*
    Pattern Matching Lists
    Lists can only be in two states 
    (1)Empty 
    (2)Cons of element onto another list

    to use match expression over these two states
*)

let list_pattern_matching nums = 
    match nums with 
    | [] -> "empty" 
    | h :: t -> "non empty" ;;

list_pattern_matching ["Hello", "world"];;

list_pattern_matching [1, 2, 3];;

(* sum of all elements in a list *)
let rec sum nums = 
    match nums with 
    | [] -> 0
    | h :: t -> h + sum t;;

sum [1; 2; 3; 4; 5];;

(* 
    to see the exact calls and returns from a function 
    use the #trace directive 
    utop # #trace sum ;;
    sum is now traced.

    utop # sum [1; 2; 3; 4; 5;] ;;
    sum <-- [1; 2; 3; 4; 5]
    sum <-- [2; 3; 4; 5]
    sum <-- [3; 4; 5]
    sum <-- [4; 5]
    sum <-- [5]
    sum <-- []
    sum --> 0
    sum --> 5
    sum --> 9
    sum --> 12
    sum --> 14
    sum --> 15
    - : int = 15

    utop # #untrace sum;;
    sum is no longer traced.
*)

(* function to get the length of the list *)
let rec len lst = 
    match lst with 
    | [] -> 0
    | h :: t -> 1 + len t;;

len [1; 2; 3; 4; 5] ;; 

(* function to combine two lists *)
let rec combine_lists lst1 lst2 = 
    match lst1 with 
    | [] -> lst2
    | h :: t -> h :: (combine_lists t lst2) ;;

combine_lists [1; 2; 3;] [4; 5; 6] ;;

(* another way to combine two lists *)
[1; 2; 3;] @ [4; 5; 6] ;;

(* syntactic sugar for list pattern matching. This can only be used when pattern matching against last arguments *)
let is_list_empty lst = function 
    | [] -> "empty" 
    | _ -> "non empty" ;; 

is_list_empty [1; 2] ;; 

is_list_empty [] ;; 


(* 
    list operators 
    @ = combines two lists = 'a list -> 'a list  -> 'a list = O(length of the list1)
    :: = prepends a element to the list = 'a -> 'a list -> 'a list = O(1) 
*)

let lst1 = [1; 2; 3;] ;; 

(* standard library methods to get the head and tail of this list *)
List.hd lst1;;

List.tl lst1;;

(* Variants = kind of enums*)
type color = Red | Blue | Green ;; (* all of these enum values should start with a capital letter*)

let clr = Red ;;

type vecf = ( float * float)

(* the below shapes is called as a variant and each shape in it is called as a Constructor in OCaml *)
type shapes = 
    (* all of these constructors should start with a capital letter*)
    (* constructors are also called as tags *)
    (* a constructor is called as constant if it carries some data and its called non constant if it doesn't carry any data *)
    | Circle of { center : vecf ; radius : float}  (* the optional data is "carried by the constructor "*)
    | Rectangle of { lower_left : vecf ; upper_right : vecf }
    | Point of vecf ;; 

let c1 = Circle { center = (2. , 4.) ; radius = 1. };;
let r1 = Rectangle { lower_left = ( 1., 1.) ; upper_right = (3. , 4.) } ;; 
let p1 = Point (20., 10.) ;; 

let avg_f a b = ((a +. b) /. 2.) ;; 

let center s = 
    match s with 
    | Circle { center; radius } -> center 
    | Rectangle { lower_left; upper_right } -> 
        let (x_ll, y_ll) = lower_left in 
        let (x_ur, y_ur) = upper_right in 
        ( (avg_f x_ll x_ur) , (avg_f y_ll y_ur) )  
    | Point (x, y) -> (x, y) ;;

let center1 s = 
    match s with 
    | Circle { center; radius } -> center 
    | Rectangle { lower_left = (x_ll, y_ll) ; upper_right = (x_ur, y_ur)} -> ((avg_f x_ll x_ur) , (avg_f y_ll y_ur)) 
    | Point p -> p ;;

(* Algebraic Data Types *)
(* A record to used to create a type *)
(* A variant is used to group records which have some connection *)
(* To express a student we would use only a record *)
(* To express multiple shapes (rectangle, triangle, square , circle ...etc) we would use a variant *)
(* record = conjunction and = a student record should contain the students name "and" age *)
(* variant = conjunction or = a shape can be rectangle or triangle or square or circle *)
(* record  and tuples = "each of" types = product type(like cartesian product) = each student will have a name(string) and a age(int) *)
(* variant = "one of" types = sum type = a shape can be one of these types (rectangle, triangle, circle, square )*)
(* variant = tagged union = union of sets *)
(* record and tuples = cartesian product of two sets *)

(* variant example *) 
type string_or_int = 
    | String of string 
    | Int of int ;; 

(* variant = tagged union as its a union of two sets and also it contains the info on which data came from which set *)
(* In string or int its easy to say which is what*)

type blue_or_red = 
    | Blue of int 
    | Red of int ;; 

(* here we can keep track of which int is from which type of ints (Blue or Red )*)
(* Sum and Product == Algebra so variants are considered to be a Algebraic Data Type *)

(* Pokemon ADT *)
type ptype = TNormal | TFire | TWater ;;

type peff = ENormal | ENotVery | ESuper ;;

let mul_of_eff p = 
    match p with 
    | ENormal -> 0.5 
    | ENotVery -> 1. 
    | ESuper -> 2.0 ;;

let eff p_a p_b = 
    match (p_a, p_b) with 
    | (TFire, TFire) -> ENormal
    | (TWater, TWater) -> ENormal
    | (TNormal, TNormal) -> ENormal
    | (TWater, TFire) -> ESuper
    | (TFire, TWater) -> ENotVery 
    | _ -> ENormal ;; 

let eff1 p_a p_b = 
    match (p_a, p_b) with 
    | (TFire, TFire) | (TWater, TWater) | (TNormal, TNormal) -> ENormal
    | (TWater, TFire) -> ESuper
    | (TFire, TWater) -> ENotVery 
    | _ -> ENormal ;; 

(* Currying by Hakell B. Curry *)

(* Variants can be recursive *)
(* Singly Linked List from scratch in OCaml *)
type intlist = 
    | Nil 
    | Cons of int * intlist ;; 

let rec len il = 
    match il with 
    | Nil -> 0 
    | Cons (_, t)-> 1 + len t
    
(* Creating a SLL *)
let il1 = Cons ( 1, Cons(2, Nil) ) ;; 

len il1 ;; 

(* Generic Linked List . In OCaml Generic is called as Parameterized Variant *)
type 'a lst = 
    | GNil 
    | GCons of 'a * 'a lst 
    
let rec g_len g_lst =
    match g_lst with 
    | GNil -> 0  
    | GCons (_ , t) -> 1 + g_len t ;; 

let sList = GCons("Hello", GCons("World", GNil )) ;; 

g_len sList ;; 

(* To have similar syntax like the OCaml builtin lists instead of constructors you can use symbols*)
(*
    type 'a lst = 
    | []
    | (::) of 'a * 'a lst 

    let rec g_len g_lst =
    match g_lst with 
    | [] -> 0  
    | _ :: t -> 1 + g_len t ;; 

    This is exactly how the standard library implements lists 

    list is a type constructor parameterized on type 'a 
    [] and :: are constructors 
*)

(* 
    Options a built in Variant 
    Think of it like a box either there is something there or it is empty 
    type 'a option = None | Some of 'a
*)

let get_val op = 
    match op with 
    | None -> failwith " it is empty "
    | Some x -> x ;;

(* we can also return some default value if there is nothing in the option *)
let get_val_def default op = 
    match op with 
    | None -> default 
    | Some x -> x 

(* Options are useful where some computation might return something some times*)
let rec list_max ( lst : 'a list) : 'a option = 
    match lst with 
    | [] -> None
    | h :: t -> 
        begin (* use begin and end with nested pattern matching *)
            match list_max t with
                | None -> Some h 
                | Some m -> Some (max h m)   
        end

(* Options solve the null pointer exception problem in Java *)
(* Exceptions in OCaml are just variants . All exceptions are of builtin exn type . exn is a builtin extensible variant *)
exception OhNo of string ;;
OhNo "oops" ;; 
(* raise is a function in OCaml std not a keyword *)
(* to raise a exception *)
(* raise (OhNo ("oops")) ;; *) 

(* there are many pre defined exceptions in OCaml std 1 / 0 *)

(* 1 / 0;; *)

(* exceptions need not carry any data *)
exception ABadThingHappend ;; 

(* raise ABadThingHappend ;;  *)

(* raise exn -> 'a ; It defined to return a option but raise never returns anything so *)
(* let x : int = raise ABadThingHappend ;; a raise can be assigned to a variable of any type as it never returns anything *)
(* Famous builtin exceptions are Failure and InvalidArgument *)
(* two ways to raise a exception raise and failwith*)
(* failwith "error occured " ;;  *)

(* the above thing is same as *)
(* raise (Failure " error occured ") ;;  *)

(* handling exceptions *)
(* basically pattern matching *)
let safe_div x y = 
    try x / y with 
        | Division_by_zero -> 0
        (* | _ -> (x / y)  *) (* only difference from pattern matching this is not necessary *)

(* Binary List *)
type 'a tree = 
    | Leaf  
    | Node of 'a * 'a tree * 'a tree

let tree1 = Node(2 , Node(1, Leaf, Leaf), Node(1, Leaf, Leaf)) ;; 

let rec num_nodes tr = 
    match tr with
    | Leaf -> 0 
    | Node(_, l, r) -> 1 + num_nodes l + num_nodes r ;; 

(* In OCaml functions are higher order / firstclass as they can passed around as values i.e. functions can return functions and functions can take other functions as arguments *)

let double x = x + x ;;

let quad double x = double (double x) ;; 

(* using the pipeline operator *)
2 |> double |> double ;; 

2 |> quad double ;;

(* apply a funciton twice*)
let twice f x = f (f x) ;; 

let quad' = twice double ;; (* returns a functions *) 

quad' 3 ;; 

(* famous higher order functions are map and reduce *)
(* map implementation from scratch *)
let rec my_map f lst = 
    match lst with 
    | [] ->  []
    | h :: t -> f h :: my_map f t   

let lst2 = [1; 2; 3] ;;

my_map double lst2 ;; 

let string_ints_lst = my_map string_of_int lst1 ;; 

(* Abstraction Principle = Factor out recurring code patterns don't duplicate them *)

(* reduce *)
let rec reduce default op lst = 
    match lst with 
    | [] -> default 
    | h :: t -> op h ( reduce default op t )

let lst_sum = reduce 0 ( + ) lst2 ;;

let lst3 = ["Hello" ; "World"] ;; 
let list_of_strings_to_string lst = reduce "" ( ^ ) lst;; 

list_of_strings_to_string lst3 ;; 

(* OCaml std has fold which is a close cousing of reduce *)

(* function with same name exists in the OCaml std *)
let rec fold_right f lst acc = 
    match lst with
    | [] -> acc 
    | h :: t -> f t ( fold_right f t acc ) ;; 

(* function with same name exists in the OCaml std *) 
let rec fold_left f acc lst = 
    match lst with 
    | [] -> acc 
    | h :: t -> fold_left f (f acc h) t ;; 

(* fold_left is tail recursive as no work is remaining to be done after the recursive call where as fold_right is not tail recursive *)
(* tail recursive functions only need constant stack space *)
(* non-tail recursive functions need stack space that is linear in the size of the list argument *)
(* In some cases you can reverse the list to apply fold_left on it rather than applying fold right on it *)

(* filter *)
(* non tail recursive version *)
let rec filter p lst = 
    match lst with 
    | [] -> []
    | h :: t -> if p h then h :: (filter p t) else (filter p t) ;; 


let is_even num = if num mod 2 == 0 then true else false ;; 

let is_odd num = 
    match ( num mod 2 ) with
    | 0 -> false 
    | _ -> true ;;
let lst4 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] ;;
filter is_even lst4 ;;
filter is_odd lst4 ;;  

(* tail recursive version *)
(* doesn't increase the asymptotic time complexity but makes the asymptotic space complexity constant *)
let rec t_filter p acc = function 
    | [] -> acc 
    | h :: t -> t_filter p (if p h then h :: acc else acc) t;;

(* only problem the elements are in the reverse order (based on their order in the original list )*)
t_filter is_even [] lst4 ;;
t_filter is_odd [] lst4 ;;  

(* to solve the reverse problem *)
let rec t_s_filter p acc = function 
    | [] -> List.rev acc (* we reverse the list *)
    | h :: t -> t_s_filter p (if p h then h :: acc else acc) t;;

t_s_filter is_even [] lst4 ;;
t_s_filter is_odd [] lst4 ;; 


(* map and fold can be used on trees too *)
let rec tr_map f node =
    match node with 
    | Leaf -> Leaf 
    | Node(v, l, r) -> Node( f v , tr_map f l , tr_map f r) ;; 

let rec tree_fold acc f node =
    match node with 
    | Leaf -> acc 
    | Node(v, l, r) -> f v (tree_fold acc f l) (tree_fold acc f r);;  

let add1 tr = tr_map (fun x -> x + 1) tr ;;

add1 tree1 ;; 

let tree_sum tr = tree_fold 0 (fun x y z -> x + y + z) tr ;;

tree_sum tree1 ;; 

tree1 |> add1 |> tree_sum ;; 

(*
    In OCaml ,
    Namespaces => Structures 
    Interfaces => Signatures 
    Encapsulation => Private Types 
    Code Re use => Functor ,  includes 
*)

(* one can write represent namespaces using module struct ..... end  syntax*)
(* module names are written in CamelCase rather than snake case in OCaml*)
(* modules can be nested *)
(* Inside modules it is not common to use ;; but it can be used *)
(* modules cannot be passed as arguments to a function *)
(* a function can't return a module *)
module Tree = struct 
    type 'a tree = 
        | Leaf 
        | Node of 'a * 'a tree * 'a tree 

    let rec len tree = 
        match tree with  
        | Leaf -> 0 
        | Node(v, l, r) -> 1 + len l + len r 
end 


module MyList = struct 
    type 'a mylist = 
    | [] 
    | Cons of 'a * 'a mylist 

    let rec len lst = 
        match lst with 
        | [] -> 0
        | Cons(h, t) -> 1 + len t 
end 

let lst5 = MyList.Cons(1, Cons(2, [])) ;; (* No need to type MyList.Cons again as it will figured out by the type inference *)
MyList.len lst5 ;; 

let tree2 = Tree.Node(2, Tree.Node(1, Leaf, Leaf), Tree.Node(3, Leaf, Leaf)) ;; 
Tree.len tree2 ;; 

(* another way of writing the above thing*)
let tree3 : int Tree.tree = Node(4, Leaf, Leaf) ;; 

module MyStack = 
    struct
        type 'a stack = 
        | Empty
        | Entry of 'a * 'a stack

        let push ele stk = Entry(ele, stk) ;; 

        let peek = function 
        | Empty -> failwith "stack is empty"
        | Entry(s , _) -> s

        let pop = function 
        | Empty -> failwith "stack is empty"
        | Entry(_, s) -> s ;; 
    end

let stk1 = MyStack.Empty ;; 

let stk1 = MyStack.push 1 stk1 ;; 

let stk1 = MyStack.push 2 stk1 ;; 

stk1 ;;

MyStack.peek stk1 ;; 

MyStack.pop stk1 ;; 

(*
    The above data structure implementations are called as functional data structures 
    There are no mutable updates 
    Funtional Data Structures are persistent rather than ephemeral 
    Functional Data Structures have a slight 
*)

let stk1 = MyStack.push 2 stk1 ;;
let stk1 = MyStack.push 3 stk1 ;;
let stk1 = MyStack.push 4 stk1 ;;

(* Syntactic Sugar for accessing module types and function *)
let stk2 = MyStack.(peek ( push 5 Empty)) ;; 

let ele1 = MyStack.(Empty |> push 1 |> peek) ;;

ele1 ;; 

(* to put a module in scope for one expression *) 

let ele2 = 
    let open MyStack in 
    Empty |> push 1 |> push 2 |> peek ;; 

ele2 ;;

(* to put a module in scope for the rest of the file *)
open MyStack ;;

let ele3 = Empty |> push 2 |> push 3 |> peek ;; 

ele3 ;; 

(* If you open two module which has somehting with a same name then one will shadow the other *)

module Queue = 
    (* maintain two lists one in correct order and the other reverse order *)
    (* list in correct order contains first half of the elements *)
    (* list in reverse order contains second half of the elements in reverse order *)
    (*
        Ex -  q = [a, b, c, d, e]
          front = [a, b]
          back  = [e, d, c]   
    *)
    struct 
        type 'a queue = {
            front : 'a list ;
            back : 'a list
        }

        let empty = {
            front = [];
            back = [];
        }

        (* If front is empty back should also be empty to guarentee that first element of the queue is at the head of front *)
        let peek = function 
        | {front = []} -> None 
        | {front = x :: _} -> Some x

        let enqueue x = function 
        | {front = []} -> {front = [x]; back=[]}
        | q -> { q with back = x :: q.back } (* this is a constant time op *)

        let dequeue x = function 
        (* O(n) *)
        | {front = []} -> None
        | {front = h :: [] ; back} -> Some {front = (List.rev back); back = []} (* O(n) *)
        | {front = h :: t; back} -> Some {front = t; back} (* O(1) *)
    end 

(* exceptions make it easier to use pipeline compared to options *)
(* we have to use another pipeline operator when functions return options rather than throw execptions *)
(* |>> *) (*called as option map*)
(* let ( |>> ) op f = 
    match op  with
    | None -> None 
    | Some x -> Some (f x) *)
(*In a pipeline full of functions returing options the results become option(option(...))*)
(*to deal with this we use >>= operator called as the bind operator*) 
(* let ( >>= ) op f = 
    match op with 
    | None -> None 
    | Some x -> f x *)

(* let ele4 = Queue.( empty |> enqueue 1 |> enqueue 2 |> dequeue |> dequeue >>= dequeue |> peek) ;;  *)

(* OCaml's equivalent of interfaces is signatures  *)
(* Unlike modules module types name need not be capitalized *)
module type Fact = 
    sig 
        val fact : int -> int    
    end

module Recursivefact : Fact = 
    struct
        let rec fact n = 
            match n with 
            | 0 -> 1 
            | _ -> n * (fact (n - 1)) 
    end 

module TailRecursiveFact : Fact = 
    struct
        let rec fact_aux n acc =
        match n with 
        | 0 -> acc 
        | _ -> fact_aux (n - 1) (n * acc) 

        let fact n = 
            fact_aux n 1

    end
(* when implementing signatures only the types , functions and value that are defined in the sinature can be accessed outside the module*)
(* Its called as module is sealed at the signature *)
let ele4 = TailRecursiveFact.fact 5 ;; 

ele4 ;; 

(* compile error*)
(* let ele5 = TailRecursiveFact.fact_aux 5 ;; 
ele5 ;;  *)

(* the types of things defined in a signature either has to be exactly matched or has to be more general *)
module type Test = sig 
  val f : int -> int 
end

module SuccTest : Test = struct
    let f x = x (* more general type than int *)
end 

(* If signature does n't have some type defintion but its implementation has it then the outisde world won't be able to access of the type*)
module type TypePrivate = sig
  val push : 'a -> 'a stack -> 'a stack
end

module TypePrivateImpl = struct 
    type 'a stack = 'a list 

    let empty = []
    let push x s = x :: s 
end

let ele5 = TypePrivateImpl.( empty |> push 42) (* works *)

(* let ele6 = TypePrivateImpl.stack = [3]*) (*doesn't work as type is private *)

(* leaving type signature from a type makes it a abstract type *)
(* abstract in the sense that its existence is declared but not defined *)
(* abstract types give us encapsulation *)

(* Compilation units *)
(* You can put the signature in a .mli file while putting its implementation in .ml file*)
(* Look into the stack folder*)
(* a pair of .mli and ml file with the same file names is called as a compilation unit *)

(* To get all encapsulation features of Compilation Units you would have to use ocaml build rather than utop*)
(* to build use the command ocamlbuild filename.cmo filename.cmi*)
(* it will be build in the _build directory*)
(* you can load this into utop *)
(* 
utop # #directory "_build" ;;

utop # #load "stack.cmo" ;; 

utop # Stack.empty ;;
- : 'a Stack.t = <abstr>

utop #  *)
(* 
    to launch utop with certain commands already run 
    you can put commands in .ocamlinit file 
    these commands are run only when you lauch utop from the directory in which .ocamlfile is in
*)

(* 
    to include 3rd party modules in utop use the command 
    #require "ounit2" ;; 
*)

(* 
    Implementation of Rings and Fields in OCaml 
*)

module Triangle = struct 
    type triangle = {
        a : int ;
        b : int ;
        c : int ;
    }
end 

module RightAngledTriangle = struct
    include Triangle (* to include everything from Triangle module *)
    let angle = 90 
end

(* include vs open *)
module Triangle1 = struct 
    type triangle = {
        a : int ;
        b : int ;
        c : int ;
    }
end 

module RightAngledTriangle1 = struct
    open Triangle1 (* just makes everything available in Triangle1 here *)
    (* open just brings things in Triangle1 to RightAngledTriangle1 scope *)
    (* open doesn't export the things it brought from Triangle1 while include dpes*)
    (* RightAngledTriangle1.triangle won't mean anything here *)
    let angle = 90 
end

(* Functor = module level function. Takes in module as a function and can produce module as a output*)
module type X = sig
  val x : int
end

module IncX = functor ( M : X ) -> struct
    let x = M.x + 1
end

(*Stdlib Map = tree map built using balanced binary trees  *)
(* Map in Stdlib has a example of functor*)

type day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday ;; 

let int_to_day = function 
    | Monday    -> 1
    | Tuesday   -> 2
    | Wednesday -> 3
    | Thursday  -> 4 
    | Friday    -> 5 
    | Saturday  -> 6 
    | Sunday    -> 7 ;;

module DayKey = struct
  type t = day 
  let compare day1 day2 = 
  int_to_day day1 - int_to_day day2
end

module DayMap = Map.Make(DayKey) ;; 

let m = let open DayMap in empty 
    |> add Monday 
    |> add Tuesday

(* assosiation lists ?? *)

module type Set = sig
  type 'a t 

  val len : 'a t -> int 

  val add : 'a -> 'a t -> 'a t 

  val remove : 'a -> 'a t -> 'a t 

  val mem : 'a -> 'a t -> bool 

end