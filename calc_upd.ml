(* Интерпретатор языка "калькулятор с переменными".
 * Программа на этом языке -- список выражений, разделенных ';'.
 * Все выражения, кроме последнего -- присваивания.
 * Последнее -- арифметическое выражение, которое могло использовать переменные
 * Пример: 
 *   x := 2;
 *   y := x + 3;
 *   x + y
 *)

(* модуль с функциями для печати в стандартный поток вывода *)
open Printf

(* тип для арифметического выражения *)
type expr = Num of int | Var of char | BinOp of char * expr * expr 

(* тип для списка присваиваний *)
type assigns = Assign of char * expr | Seq of assigns * assigns 

(* алиас типа для программ языка калькулятора *)
type calc = assigns * expr

(* построение строкового представления выражения *)
let string_of_expr x = 
  let rec print = function 
  | Num i     -> sprintf "%d" i
  | Var c     -> sprintf "%c" c
  | BinOp (op, x, y) ->
    match op with 
    (* Comment on using parentheses *)
    | '+' -> sprintf "(%s) + (%s)" (print x) (print y)
    | '-' -> sprintf "(%s) - (%s)" (print x) (print y)
    | '*' -> sprintf "(%s) * (%s)" (print x) (print y)
    | '/' -> sprintf "(%s) / (%s)" (print x) (print y)
    | _ -> failwith "Unknown operation!"
  in sprintf "%s" (print x)

(* печать выражения в стандартный поток вывода *)
let print_expr x = printf "%s\n%!" (string_of_expr x)

(* построение сторокового представления списка присваиваний *)
let string_of_assign x = 
  let rec print = function
  | Assign (x, e) -> sprintf "%c := %s" x (string_of_expr e)
  | Seq (x, y)    -> sprintf "%s;\n%s" (print x) (print y)
  in sprintf "%s" (print x)

(* печать списка присваиваний в стандартный поток *)
let print_assigns x = printf "%s\n%!" (string_of_assign x)

(* построение строкового представления программы *)
let string_of_calc (s, e) = sprintf "%s;\n%s" (string_of_assign s) (string_of_expr e)

(* печать программы в стандартный поток *)
let print_calc x = printf "%s\n%!" (string_of_calc x)

(* Вычисление целочисленного значения выражения e
 * env -- environment, список пар (переменная, значение), 
 * где значение -- значение выражения, присвоенного данной переменной где-то ранее
 *)
let eval_expr env e = 
  let rec eval env = function
  | Num i     -> i
  | Var v     -> (try List.assoc v env with Not_found -> failwith "Unknown variable!")
  | BinOp (op, x, y) ->
    match op with
    | '+' -> (eval env x) + (eval env y)
    | '-' -> (eval env x) - (eval env y)
    | '*' -> (eval env x) * (eval env y)
    | '/' -> (eval env x) / (eval env y)
    | _   -> failwith "Unknown operation!"
  in eval env e

(* Вычисление "значения" списка присваиваний.
 * В данном случае значением является environment, 
 * используемый для вычисления значений выражений
 *)
let eval_assigns s = 
  let rec eval env = function 
  | Assign (x, e) -> (x, eval_expr env e) :: env
  | Seq (x, y) -> let env' = eval env x in eval env' y
  in eval [] s 

(* Вычисление значения программы 
 * Сначала обрабатываем список присваиваний, 
 * потом вычисляем значение финального выражения в этом окружении 
 *)
let eval_calc (s, e) = eval_expr (eval_assigns s) e

(* основная функция, несколько примеров *)
let _ =
  printf "%s\n" "TEST 0: ";
  let e0 = Num 1 in  
  let e1 = Var 'c' in
  let e2 = BinOp ('-', BinOp ('+', Num 123, Var 'x'),Num 23) in
(*
  printf "%s" "e0: ";
  print_expr e0;
  printf "%s" "e1: ";
  print_expr e1;
  printf "%s" "e2: ";
  print_expr e2;
  printf "\n";	  
*)
  let s0 = Assign ('x', e0) in
  let s1 = Assign ('c', (BinOp ('*', Num 2, Num 1))) in
  let s2 = Assign ('y', e1) in
  let s3 = Seq (s0, (Seq (s1, Seq (s2, Assign ('z', e2))))) in
  
(*  printf "\n";
  printf "%s" "s0: ";
  print_assigns s0; 
  printf "%s" "s1: ";
  print_assigns s1; 
  printf "%s" "s2: ";
  print_assigns s2; 
  printf "%s" "s3: \n";
  print_assigns s3; 
  printf "\n";
*)
   
  let c0 = s3, (BinOp ('/', e2, BinOp ('+', e1, Var 'y'))) in 
  print_calc c0 ;
  printf "\nAnswer: %d\n%!" (eval_calc c0);
  printf "\n";
  (*---------------------------------------------------------------------------------*)
  printf "%s\n" "TEST 1: ";
  let e3 = Var 'x' in 
  let s00 = Assign ('x', e0) in (* x := 1 -> x = 1*)
  let s01 = Assign ('y', e3) in (* y := x -> y = 1 *) 
  let s02 = Assign ('x', (BinOp ('*', Var 'x', Num 2))) in (* x:= x*2 -> x = 2 *)
  let s03 = Seq (s00, (Seq (s01, Seq (s02, Assign ('y', (BinOp ('-', e3, Num 5))))))) in 
(*
  printf "%s" "s00: ";
  print_assigns s00;  
  printf "%s" "s01: ";
  print_assigns s01;
  printf "%s" "s02: ";
  print_assigns s02;
  printf "%s" "s03: ";
  print_assigns s03;
*) 
  let c1 = s03, (BinOp ('-', BinOp ('-', Num 0, e3), Var 'y')) in 
  print_calc c1;
  printf "\nAnswer: %d\n%!" (eval_calc c1);
  printf "\n";
  (*---------------------------------------------------------------------------------*)
  printf "%s\n" "TEST 2: ";
  let s04 = Seq (s00, (Seq (s01, Seq (s02, Assign ('y', (BinOp ('-', e3, Num 5))))))) in
  let c2 = s04, (BinOp ('-', BinOp ('-', Num 0, e3), Var 'F')) in 
  print_calc c2;
  printf "\nAnswer: %d\n%!" (eval_calc c2);
