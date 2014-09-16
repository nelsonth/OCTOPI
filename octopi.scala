import scala.util.parsing.combinator._
import org.kiama.output.PrettyPrinter
import org.kiama.attribution.Attribution._
import org.kiama.attribution.Attribution
import org.kiama.attribution.Attributable
import java.io._
import scala.collection.immutable.Seq

object OctoTree {
	import org.kiama.util.TreeNode

	sealed trait OctoTree extends TreeNode  // the Octopi Node type for everything


	sealed abstract class Exp extends OctoTree

	case class Tensor(name: String, ind: Seq[String]) extends Exp

	case class Mul(lhs: Exp, rhs: Exp) extends Exp


	sealed abstract class Stmt extends OctoTree

	// eventually bounds should be something else
	case class Loop(body: Stmt, ivar: String, bounds: String) extends Stmt 

	case class Assign(lhs: Tensor, rhs: Exp) extends Stmt // lhs = rhs


	// Later we will expand on this, at the very least a list of statements
	sealed abstract class Program extends OctoTree

	case class Sum(decl: Seq[Tensor], lhs: Tensor, ind: Seq[String], rhs: Exp) extends Program

	case class Code(decl: Seq[Tensor], stmt: Stmt) extends Program

	// Each index is actually the size of that dimension
	def containerSizes(t: Tensor) : (String, List[String]) = {
		t match {
			case Tensor(name, ind) =>
				return (name, ind.foldRight(List(""))(
					(i, xs) => (xs.head + "*" + i)::xs))
		}
	}

}

trait SyntaxAnalyser extends JavaTokenParsers {

	import OctoTree._
	
	def indices : Parser[List[String]] = 
        "[" ~> rep(ident) <~ "]"
        
    def tensor : Parser[Tensor] =
        ident ~ indices ^^ {case n ~ i => Tensor(n,i)}

	def term : Parser[Exp] = 
		factor ~ opt( "*" ~ factor ) ^^ //(x => x match
			{
				case l ~ Some("*" ~ r) => Mul(l,r)
				case l ~ None => l
			}
	
	def factor : Parser[Exp] = 
		tensor | 
		"(" ~ term ~ ")" ^^ {case "(" ~ t ~ ")" => t}

	/*
	def stmt : Parser[Sum] = 
		tensor ~ "= sum(" ~ indices ~ "," ~ term <~ ")" ^^ 
			{case lhs ~ "= sum(" ~ i ~ "," ~ t => Sum(lhs, i, t)}
	*/
	
	// Top Level Program = a single sum (for now)
	def program : Parser[Sum] = 
		rep(tensor) ~ "{" ~ tensor ~ "= sum(" ~ indices ~ "," ~ term <~ ")" <~ "}" ^^ 
			{case d ~ "{" ~ lhs ~ "= sum(" ~ ind ~ "," ~ rhs => Sum(d, lhs, ind, rhs)}
}

object CPrinter extends PrettyPrinter {
	import OctoTree._

	def pretty(t : OctoTree) : String = {
		super.pretty(show(t))
	}
	
	def show(t : OctoTree) : Doc = {

		t match {
			case Tensor(name,ind) => name <> "[" <> flattenAccess(ind, "") <> "]"
			case Mul(lhs, rhs) => show(lhs) <> "*" <> show(rhs)
			case Sum(decl, lhs, ind, rhs) => {
				"Declarations: " <> sep(decl.map(show)) <+> show(lhs) <> "= Sum(" <+>  ssep(ind map text, "][") <> "," <> show(rhs) <> ")"
			}

			case Assign(lhs, rhs) => show(lhs) <> "=" <> show(rhs)

			case Loop(body, ivar, bounds) => "for (" <> bounds <> ") {" <@> show(body)  <@> "}"

			case Code(decl: Seq[Tensor], stmt: Stmt) => "void nek(double *" + functionArgs(decl) + ") {" <@> show(stmt) <@> "}"

			// case _ => throw new RuntimeException("unrecognized case in PrettyPrint")
			/*
			case Addr(indx, sub) => sub match {
				case None => indx
				case Some(a) => indx <+> show(a)
			}
			*/
		}
	}

	def indexLoop(i:String) = "for (int "+i+"=0; " + i + "<10; ++" + i + ") {"

	def functionArgs(vars: Seq[Tensor]) = vars.map(_.name).mkString(", double* ")

	def flattenAccess(ind: Seq[String], stride: String) : String = ind match {
		case Seq() => return ""
		case x :: xs => x + stride + " + " + flattenAccess(xs, stride + "*" + x + "_max")
	}
}

/*
trait Evaluator {
	import OctoTree._
	import org.kiama.rewriting.Rewriter._

	val tensor_rewrite = // should be (ONode => Option[ONode]) , but can't typecheck
		rule[ONode] {
			case Tensor(name, ind) => IndTensor(name, ind.head)
		}	<+ id // in all other cases we do nothing and pass

		//	<+ (id:(ONode => Option[ONode]))  this cast doesn't work, can't typecheck the strategies
		
	
	val change_all_tensors = topdown(tensor_rewrite)

}
*/

trait Evaluator {
	import OctoTree._

	def express(s: Sum) : Code = {
		s match {
			case Sum(decl, lhs, ind, rhs) => 
				Code(decl, lowerLoops(ind ++ lhs.ind, lhs, rhs))
			// case _ => throw new RuntimeException("unrecognized case in express")
		}
	}

	def lowerLoops(indexList : Seq[String], lhs: Tensor, rhs: Exp) : Stmt = {
		indexList match {
			case Seq() => Assign(lhs, rhs)
			case x :: xs => Loop(lowerLoops(xs, lhs, rhs), x, "int " + x + " = 0; " + x + " < 10; ++" + x)
		}
	}
}

// object OCTOPI extends SyntaxAnalyser with Evaluator {
object OCTOPI extends SyntaxAnalyser with Evaluator {

	import OctoTree._
	import org.kiama.rewriting.Rewriter.rewrite

	def main(args: Array[String]) {

		import java.io.FileReader

		val reader = new FileReader(args(0))
		val parser_result = parseAll(program, reader)
		parser_result match {
			case Success(program,i) => {
				println(program.decl.map(OctoTree.containerSizes _))
				// rewrite tries to apply the rewrite rule, 
				// if it fails you get the original term back
				// val s = rewrite[ONode](change_all_tensors)(program.code)

				println("==  Before Lowering ==")
				println(program)
				println(CPrinter.pretty(program))
				val lowered = express(program)
				println("== After Lowering ==")
				println(lowered)
				println(CPrinter.pretty(lowered))
				val output = new FileWriter("tuned.c")
				output.write(CPrinter.pretty(lowered))
				output.close()
				println("== wrote file to tuned.c, compile and test! ==")
			}
			case Failure(m,i) => println(m)
			case Error(m,i) => println(m)
		}
	}
}
