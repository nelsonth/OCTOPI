import scala.util.parsing.combinator._
import org.kiama.output.PrettyPrinter
import org.kiama.attribution.Attribution._
import org.kiama.attribution.Attribution
import org.kiama.attribution.Attributable

object OctoTree {
	import org.kiama.util.TreeNode

	abstract class ONode extends TreeNode

    case class Tensor(name: String, ind: List[String]) extends ONode

	case class Mul(lhs: ONode, rhs: ONode) extends ONode

	case class Sum(lhs: ONode, ind: List[String], rhs: ONode) extends ONode

	case class Program(decl: List[Tensor], code: ONode) extends ONode

	case class Addr(indx: String, sub: Option[Addr]) extends ONode

	case class IndTensor(name: String, ind: String) extends ONode

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

	def term : Parser[ONode] = 
		factor ~ opt( "*" ~ factor ) ^^ //(x => x match
			{
				case l ~ Some("*" ~ r) => Mul(l,r)
				case l ~ None => l
			}
	
	def factor : Parser[ONode] = 
		tensor | 
		"(" ~ term ~ ")" ^^ {case "(" ~ t ~ ")" => t}

	def stmt : Parser[Sum] = 
		tensor ~ "= sum(" ~ indices ~ "," ~ term <~ ")" ^^ 
			{case lhs ~ "= sum(" ~ i ~ "," ~ t => Sum(lhs, i, t)}
	
	// Top Level Program
	def program : Parser[Program] = 
		rep(tensor) ~ "{" ~ stmt <~ "}" ^^ {case d ~ "{" ~ s => Program(d, s)}
}

object CPrinter extends PrettyPrinter {
	import OctoTree._

	def pretty(t : ONode) : String = {
		super.pretty(show(t))
	}
	
	def show(t : ONode) : Doc = {
		t match {
			case Tensor(name,ind) => name <> "[" <> sep(ind map text) <> "]"
			case IndTensor(name,ind) => name <> "[" <> ind  <> "]"
			case Mul(lhs, rhs) => show(lhs) <> "*" <> show(rhs)
			case Sum(lhs, ind, rhs) => {
				// val loopIndices = lhs.ind ::: ind
				return sep(ind.map(indexLoop) map text) <@>
					show(lhs) <+> "=" <+> show(rhs) <@> 
					sep(ind.map(_ => "}") map text)
			}
			case Program(decl, code) => 
				("void axpy(double* "+ decl.mkString(", double* ") + ") {\n") <> show(code) <> "\n}"
			case Addr(indx, sub) => sub match {
				case None => indx
				case Some(a) => indx <+> show(a)
			}
		}
	}

	def indexLoop(i:String) = "for (int "+i+"=0; " + i + "<10; ++" + i + ") {"
}

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


object OCTOPI extends SyntaxAnalyser with Evaluator {

	import OctoTree._
	import org.kiama.rewriting.Rewriter.rewrite

	def main(args: Array[String]) {

		import java.io.FileReader

		val reader = new FileReader(args(0))
		val parser_result = parseAll(program, reader)
		parser_result match {
			case Success(program,i) => {
				println(program.decl.map(OctoTree.containerSizes))
				// rewrite tries to apply the rewrite rule, if it fails you get the original term back
				val s = rewrite[ONode](change_all_tensors)(program.code)
				println(program.code)
				println(s)
				println(CPrinter.pretty(program.code))
				println(CPrinter.pretty(s))
			}
			case Failure(m,i) => println(m)
			case Error(m,i) => println(m)
		}
	}
}
