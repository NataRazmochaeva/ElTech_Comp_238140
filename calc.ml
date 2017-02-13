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

(* тип для арифметического выражения 
   Просто добавлены конструкторы для каждой операции *)
type expr = Num of int | Var of char | Add of expr * expr | Min of expr * expr | Mul of expr * expr | Div of expr * expr

(* тип для списка присваиваний *)
type assigns = Assign of char * expr | Seq of assigns * assigns 

(* алиас типа для программ языка калькулятора *)
type calc = assigns * expr

(* построение строкового представления выражения *)
let string_of_expr x = 
  let rec print = function 
  | Num i     -> sprintf "%d" i
  | Var c     -> sprintf "%c" c
  | Add (x,y) -> sprintf "(%s) + (%s)" (print x) (print y) 
  | Min (x,y) -> sprintf "(%s) - (%s)" (print x) (print y) 
  | Mul (x,y) -> sprintf "(%s) * (%s)" (print x) (print y) 
  | Div (x,y) -> sprintf "(%s) / (%s)" (print x) (print y) 
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
  | Var v     -> List.assoc v env
  | Add (x,y) -> eval env x + eval env y
  | Min (x,y) -> eval env x - eval env y
  | Mul (x,y) -> eval env x * eval env y
  | Div (x,y) -> eval env x / eval env y
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
  let e0 = Num 1 in  
  let e1 = Var 'c' in
  let e2 = Min (Mul (Num 123, Var 'x'),Num 23) in
  (*
  print_expr e0;
  print_expr e1;
  print_expr e2;
  *)
  let s0 = Assign ('x', e0) in
  let s1 = Assign ('c', (Mul (Num 2, Num 1))) in
  let s2 = Assign ('y', e1) in
  let s3 = Seq (s0, (Seq (s1, Seq (s2, Assign ('z', e2))))) in
  (*
  printf "\n";
  print_assigns s0; printf "\n";
  print_assigns s1; printf "\n";
  print_assigns s2; printf "\n";
  print_assigns s3; printf "\n";
  *)

  let c0 = s3, (Mul (e2, Add (e1, Var 'y'))) in 

  print_calc c0 ;
  
  printf "\n%d\n%!" (eval_calc c0);
