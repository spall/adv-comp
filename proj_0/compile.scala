/* 
   First scala code ever

   Author: Sarah Spall
   
   Code intended to compile 
   
   <exp> ::= <var>

        |  #t
        |  #f
        |  (if  <exp> <exp> <exp>)
        |  (and <exp> <exp>)
        |  (or  <exp> <exp>)

        |  <nat>
        |  (zero? <exp>)
        |  (- <exp> <exp>)
        |  (= <exp> <exp>)
        |  (+ <exp> <exp>)
        |  (* <exp> <exp>)

        |  <lam>
        |  (let ((<var> <exp>) ...) <exp>)
        |  (letrec ((<var> <lam>)) <exp>)

        |  (cons <exp> <exp>)
        |  (car  <exp>)
        |  (cdr  <exp>)
        |  (pair? <exp>)
        |  (null? <exp>)
        |  '()

        |  (<exp> <exp> ...)

 <lam> ::= (λ (<var> ...) <exp>)

       to
	
	<exp> ::= <var>
             |  (<exp> <exp>)
             |  (λ (<var>) <exp>)

*/

def compile(exp : Exp) {
	exp match {
	    case RefExp(id) => RefExp(id)
	    case BoolExpr(val) =>
	    if(val) /* true */
	    {
		LambdaExp("t", LambdaExp("f", AppExp(
	    }
	    else
	    {
		
	    }
	    case IfExp(cond, t, f) =>
	    	 compile(AppExp(cond, 
	    case AndExp(cond1, cond2) =>
	    	 compile(IfExp(cond1, cond2, BoolExpr(false)))
	    case OrExp(cond1, cond2) =>
	    	 compile(IfExp(cond1, BoolExpr(true), cond2))
	    case IntExp(val)
	    case ZeroPExp(test)
	    case SubExp(exp1, exp2)
	    case PlusExp(exp1, exp2)
	    case TimesExp(exp1, exp2)
	    case EqExp(exp1, exp2)
	    case NullExp()
	    case ConsExp(car,cdr)
	    case CarExp(pair)
	    case CdrExp(pair)
	    case PairPExp(arg)
	    case NullPExp(arg)
	    
	    case LambdaExp(params, body) =>
	    	 if(args.isEmpty)
		 {
			LambdaExp(params,compile(body))
		 }
		 else if(args.tail.isEmpty)
		 {
			LambdaExp(params, compile(body))
		 }
		 else
		 {
			LambdaExp(params.head, compile(LambdaExp(params.tail, body)))	
		 }
	    
	    case LetExp(vars, exps, body)
	    case LetRecExp(fun, lam, body)
	    
	    case AppExp(fun,args) =>
	    	 if(args.isEmpty) /* void */
		 {
			compile(AppExp(compile(fun), args)) /* not sure about this one */
		 }	    
		 else if(args.tail.isEmpty)
		 {
			AppExp(compile(fun), compile(arg.head))
		 }
		 else
		 {
			compile(AppExp(AppExp(fun, args.head),args.tail))
		 }		 

}	    