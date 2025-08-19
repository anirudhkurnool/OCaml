type 'a t = 'a list 

let empty = []

let is_empty stk = if stk = empty then true else false

let push ele stk = ele :: stk 

let pop stk = 
  match stk with 
  | [] -> failwith "stack is empty"
  | _ :: t -> t

let peek stk = 
  match stk with 
  | [] -> failwith "stack is empty"
  | h :: _ -> h 