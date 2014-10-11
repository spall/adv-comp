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
	    if(val) /* true */ /* true and false return lambdas */
	    {
		true()
	    }
	    else /* need to define void */
	    {
		false()
	    }
	    case IfExp(cond, t, f) =>
	    	 compile(AppExp(cond, LambdaExp(List(), t), LambdaExp(List(), f)))
	    case AndExp(cond1, cond2) =>
	    	 compile(IfExp(cond1, cond2, BoolExpr(false)))
	    case OrExp(cond1, cond2) =>
	    	 compile(IfExp(cond1, BoolExpr(true), cond2))
	    case IntExp(val) => church_numeral(val)
	    case ZeroPExp(test) => AppExp(zero_huh(), compile(test))
	    case SubExp(exp1, exp2) => AppExp( AppExp(sub(), compile(exp1)), compile(exp2))
	    case PlusExp(exp1, exp2) => AppExp( AppExp(sum(), compile(exp1)), compile(exp2))
	    case TimesExp(exp1, exp2) => AppExp( AppExp(mul(), compile(exp1)), compile(exp2))
	    case EqExp(exp1, exp2) => compile( AndExp( AppExp(zero_huh(), SubExp(exp1, exp2)),
	    	 	     	      	       	       AppExp(zero_huh(), SubExp(exp2, exp1))))
	    case NullExp() =>
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

def apply_n(f, n, z) /* f(f(f(z) thing */
{
	if( n == 0)
	    z
	else
	{	
	AppExp(f, apply_n(f, n-1, z))	
		}
}
def church_numeral(n)
{
	if(n == 0)
	{
		LambdaExp(f, LambdaExpr(z, z))
	}	
	else
	{
		LambdaExp(f, LambdaExpr(z, apply_n(f n z)))	
	}

} 

def true()
{
	LambdaExp(List("t"), LambdaExp(List("f"), AppExp( RefExp("t"), void())))
}

def false()
{
	LambdaExp(List("t"), LambdaExp(List("f"), AppExp(RefExp("f"), void())))
}

def zero_huh()
{
	LambdaExp(List("n"), AppExp( AppExp( RefExp("n"), LambdaExp(List(), false())), true()))
}

def sum()
{
	LambdaExp(List("n"), 
		LambdaExp(List("m"), 
			LambdaExp(List("f"), 
				LambdaExp(List("z"), 
					AppExp( AppExp( 
						RefExp("m"), 
						RefExp("f")), 
					  AppExp( AppExp(RefExp("n"), 
							     	RefExp("f")), 
								RefExp("z")))))))
}

def mul()
{
	LambdaExp(List("n"),
		LambdaExp(List("m"),
			LambdaExp(List("f"),
				LambdaExp(List("z"),
					AppExp( AppExp(RefExp("m"), AppExp(RefExp("n"), RefExp("f"))),
						RefExp("z"))))))
}

def pred()
{
	LambdaExp(List("n"),
		LambdaExp(List("f"),
			LambdaExp(List("z"),
				AppExp( AppExp( AppExp(RefExp("n"), LambdaExp(List("g"),
										LambdaExp(List("h"),
											AppExp(RefExp("h"),
												AppExp(RefExp("g"), RefExp("f"))))))
							LambdaExp(List("u"), RefExp("z")))
					LambdaExp(List("u"), RefExp("u"))))))

}

def sub()
{
	LambdaExp(List("n"), LambdaExp(List("m"), AppExp( AppExp(RefExp("m"), pred()), RefExp("n"))))
}