//
// Analyzer for simple C programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
// Modified by:
//   << Kenan Alghythee >>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //
   //
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

  let rec contains_dup  L = 
    
    match L with 
    | [] -> false 
    | head::tail when contains head tail = true ->  true      
    | head::tail -> contains_dup tail

  let rec private expr_value tokens =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      T2
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      T2
    //
    // the others are trickier since we have to look 
    // at the start of the string for a match:
    //
    elif next_token.StartsWith("identifier") then
      let T2 = matchToken "identifier" tokens
      T2
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      T2
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      T2
    elif next_token.StartsWith("real_literal") then  // changes made here 
      let T2 = matchToken "real_literal" tokens
      T2
    else
      tokens


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
  let rec private expr_op tokens = 
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
  let rec private expr tokens  = 
    //
    // first we have to match expr-value, since both
    // rules start with this:
    //
    let T2 = expr_value tokens
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
      let T4 = expr_value T3
      T4
    else
      // just expr_value, that's it
      T2


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
      if contains added_var symboltable = true then 
        failwith("redefinition of variable '" + new_var_name + "'")
      else 
       let T3 = matchToken "identifier" T2
       let T4 = matchToken ";" T3
       (T4,added_var::symboltable)
    else  
      let T2 = matchToken "real" tokens
      let var_name = List.head T2
      let new_var_name = var_name.Substring(11)
      let added_var = (new_var_name,"real")
      if contains added_var symboltable = true then 
        failwith("redefinition of variable '" + new_var_name + "'")
      else 
       let T3 = matchToken "identifier" T2
       let T4 = matchToken ";" T3
       (T4,added_var::symboltable)
      
    

  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
   
        
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let T2 = expr_value tokens
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens  = 
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3
    let T5 = matchToken ";" T4
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
      let T2 = input tokens
      (T2,symboltable)
    elif next_token = "cout" then
      let T2 = output tokens
      (T2,symboltable)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens
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
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let (T6,table) = then_part T5 symboltable
    let (T7,table2) = else_part T6 table
    (T7,table2)
  //
  // <condition> -> <expr>
  //
  and private condition tokens = 
    let T1 = expr tokens
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
  let private simpleC tokens  = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7,symboltable) = stmts T6 []
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8  // $ => EOF, there should be no more tokens
    (T9,symboltable)







  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
