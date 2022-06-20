//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
//
// Modified by:
//   << Kenna alghythee >>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //
    // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    
    List.tail tokens
    

  //
  // <expr-value> -> identifier
  //               | int_literal
  //               | str_literal
  //               | true
  //               | false
  //
  // ** contains function : similar to the one in the lecutre and just determin if an 
    // element in the list or not ***
  let rec contains (x,y) L = 
    match L with 
    | [] -> false 
    | (name,the_type)::tail when name = x -> true                   
    | head::tail -> contains (x,y) tail
    
  let rec contains_type (x,y) L = 
    match L with 
    | [] -> "issue" 
    | (name,the_type)::tail when name = x -> the_type            
    | head::tail -> contains_type (x,y) tail
 



 /////////////////////////////////////////////////////////
  let rec private expr_value tokens symboltable  =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      (T2,"bool" )
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      (T2,"bool")
    //
    // the others are trickier since we have to look 
    // at the start of the string for a match:
    //
    elif next_token.StartsWith("identifier") then
      let var_name = next_token.Substring(11)
      let T2 = matchToken "identifier" tokens
      if contains (var_name,"type") symboltable = false then  
        failwith("variable '" + var_name + "' undefined")
      else
        let the_type = contains_type (var_name,"type") symboltable
        (T2,the_type)
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      (T2,"int")
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      (T2,"str")
    elif next_token.StartsWith("real_literal") then  // changes made here 
      let T2 = matchToken "real_literal" tokens
      (T2,"real")
    else
      (tokens,"not")


  //
  // <expr-op> -> +
  //            | -
  //            | *
  //            | /
  //            | ^
  //            | <
  //            | <=
  //            | >
  //            | >=
  //            | ==
  //            | !=
  //
  let rec private expr_op tokens  = 
    let next_token = List.head tokens
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T2 = matchToken next_token tokens
      T2
    else
      // error
      tokens 


  //
  // <expr> -> <expr-value> <expr-op> <expr-value>
  //         | <expr-value>
  //
  let rec private expr tokens symboltable  = 
    //
    // first we have to match expr-value, since both
    // rules start with this:
    //
    let (T2,the_type_left) = expr_value tokens symboltable
    //
    // now let's see if there's more to the expression:
    //
    let next_token = List.head T2
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T3 = expr_op T2
      let (T4,the_type_right) = expr_value T3 symboltable
      // handle the case of opreations (case 3 in the PDF)
      if next_token = "+" 
        || next_token = "-"
        ||next_token = "*"||
        next_token = "/" || 
        next_token = "^"   then 

        if (the_type_left = "real" || the_type_left = "int") && (the_type_left = the_type_right) then 
          (T4,the_type_left)
        else
          failwith("operator " + next_token + " must involve 'int' or 'real'")
      ////////////////////// the case of number four( 4) in the PDF) 
      else // this is used to handle the condation (4) of type mismatch
        if next_token = "<"  || next_token = "<="||next_token = ">"|| next_token = ">="||next_token = "=="||next_token = "!=" then
          if (next_token = "==") && (the_type_left = "real") && (the_type_right = "real") then 
             printfn "warning: comparing real numbers with == may never be true"
          if  (the_type_left = the_type_right)  then 
           (T4,"bool")
          else
            failwith("type mismatch '" + the_type_left + "' "+ next_token + " '"+ the_type_right +  "'")
        else
           (T4,"Issue,wrong type")
     else
      // just expr_value, that's it
       (T2,the_type_left)


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2


  //
  // <vardecl> -> int identifier ;
  //
  let rec private vardecl tokens symboltable = 
    let the_type:string = List.head tokens 
    if the_type = "int" then
      let T2 = matchToken "int" tokens
      let var_name = List.head T2
      let new_var_name = var_name.Substring(11)
      let added_var = (new_var_name,"int")
      
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      (T4,added_var::symboltable)
    else  
      let T2 = matchToken "real" tokens
      let var_name = List.head T2
      let new_var_name = var_name.Substring(11)
      let added_var = (new_var_name,"real")
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      (T4,added_var::symboltable)
      
    

  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens table = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    // checking for the defiantion 
    let checking = List.head T3
    if contains (checking.Substring(11), "type") table = false then 
      failwith("variable '" + checking.Substring(11) + "' undefined")
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens symboltable = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let (T2,the_type) = expr_value tokens symboltable
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens symboltable   = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 symboltable
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens symboltable = 
    let hd:string = List.head tokens 
    let var_name = hd.Substring(11)
    //getting the type of the X(left side )
    let the_type_x = contains_type (var_name,"type") symboltable
    if the_type_x = "issue" then 
      failwith("variable '" + var_name + "' undefined")

    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let (T4,the_type_y) = expr T3 symboltable
    let T5 = matchToken ";" T4
    if (the_type_x = the_type_y) || (the_type_x = "real") && (the_type_y = "int") then 
      let result = T5
      result |> ignore
    else
      failwith("cannot assign '" + the_type_y + "' to variable of type '" + the_type_x + "'")
    if contains (var_name,"type") symboltable = false then      
      failwith("variable '" + var_name + "' undefined")
    else
      T5


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens symboltable = 
   
    //
    // use the next token to determine which rule
    // to call; if none match then it's a syntax
    // error:
    //
    let next_token = List.head tokens
    if next_token = ";" then
      let T2 = empty tokens
      (T2, symboltable)
    elif next_token = "int" then  // changes here
      let (T2,table) = vardecl tokens symboltable
      (T2, table)
    elif next_token = "real" then  // changes here
      let (T2,table) = vardecl tokens symboltable
      (T2,table)
    elif next_token = "cin" then
      let T2 = input tokens symboltable
      (T2,symboltable)
    elif next_token = "cout" then
      let T2 = output tokens symboltable
      (T2,symboltable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens symboltable
      (T2,symboltable)
    elif next_token = "if" then
      let (T2,table) = ifstmt tokens symboltable
      (T2,table)
    else
      (tokens,symboltable) 
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens symboltable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 symboltable
    let T5 = matchToken ")" T4
    let (T6,table) = then_part T5 symboltable
    let (T7,table2) = else_part T6 table
    (T7,table2)
  //
  // <condition> -> <expr>
  //
  and private condition tokens symboltable = 
    let (T1,the_type) = expr tokens symboltable
    if the_type <> "bool" then 
      failwith("if condition must be 'bool', but found '" + the_type + "'")
    else 
      T1 
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symboltable = 
    let (T1,table) = stmt tokens symboltable
    (T1,table)
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symboltable = 
  
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3,table) = stmt T2 symboltable
      (T3,table)
    else
      // EMPTY, do nothing but return tokens back
      (tokens,symboltable)

   


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts tokens symboltable = 
    //
    // if the next token denotes the start of a stmt 
    // then process stmt and morestmts, otherwise apply
    // EMPTY
    //
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "real" ||
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
      //
      let (T2,table) = stmt tokens symboltable
      let (T3,table2) = morestmts T2 table
      (T3,table2)
    else 
      // EMPTY => do nothing, just return tokens back
      (tokens, symboltable)
    


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symboltable = 
    let (T2,table) = stmt tokens symboltable
    let (T3,table_2) = morestmts T2 table
    (T3,table_2)


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens symboltable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7,table) = stmts T6 symboltable
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF, there should be no more tokens
    (T9,table)

  


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

