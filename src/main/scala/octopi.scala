import scala.util.parsing.combinator._
import org.kiama.output.PrettyPrinter
import org.kiama.attribution.Attribution._
import org.kiama.attribution.Attribution
import org.kiama.attribution.Attributable
import java.io._
import scala.collection.immutable.List
import scala.collection.immutable.Set

object OctoTree {
    import org.kiama.util.TreeNode

    // Expressions in tensor language
    sealed abstract class Exp extends TreeNode

    case class Tensor(name: String, ind: Set[String]) extends Exp

    case class Mul(res:Tensor, lhs: Exp, rhs: Exp) extends Exp

    case class Sum(res:Tensor, ind: String, rhs: Exp) extends Exp

    // Program object for parser
    case class Program(lhs: Tensor, rhs: List[Tensor])

    // prints tensors in the string format Axel needs
    def varDecl(t : Tensor) : String = t match {
        case Tensor(n, i) => n + ":(" + i.map(_.toUpperCase).mkString(",") + ")"
    }

    def ispace(exp:Exp) : Set[String] = exp match {
            case Tensor(n, i) => i
            case Mul(res,l,r) => res.ind
            case Sum(res,i,r) => res.ind
        }

    def lval(exp:Exp) : Tensor = exp match {
        case Tensor(n,i) => Tensor(n,i)
        case Mul(res,l,r) => res
        case Sum(res,i,r) => res
    }

    // find all the temporaries used in the computation
    def tensorTemps(e : Exp) : Set[Tensor] = e match {
        case Tensor(n,i) => Set(Tensor(n,i))
        case Mul(res, lhs, rhs) => tensorTemps(lhs).union(tensorTemps(rhs)) // + res not storing in mults for now
        case Sum(res, i, rhs) => tensorTemps(rhs) + res
    }

    def implement(tempV:Int, lhs: Set[String], pre: List[Exp],  post: List[Exp]) : List[Exp] = {
        //println("pre = " + pre + ", size = " + pre.size)
        //println("post = " + post + ", size = " + post.size)
        require(post.size > 0)
        for (i <- (0 to post.size-1)) {
            val e = post(i)
            // fold to get the set of unique indices, indices that appear nowhere else
            // &~ = left set difference
            val uniqs = ((ispace(e) &~ lhs) /: (pre ++ post).filter(_ != e)) (
                { case (a,x) => a &~ ispace(x) })
            if (uniqs.size > 0) {
                // just take the first
                val next = Sum(Tensor("temp" + tempV.toString, ispace(e) - uniqs.head),
                    uniqs.head, e)
                return implement(tempV + 1, lhs, (pre ++ post.take(i)).filter(_ != e),
                    next :: post.drop(i + 1))
            }
        }
        if ((pre.size + post.size) == 1) {
            // done, singleton list
            return post
        } else {

            var results = List[Exp]()
            for (i <- (0 to post.size-1); a <- pre ++ post.take(i)) {
                val next = Mul(Tensor("temp" + tempV.toString,
                    ispace(a).union(ispace(post(i)))), a, post(i))
                results = results ++ implement(tempV + 1,lhs,
                    (pre ++ post.take(i)).filter(_ != a), next::post.drop(i + 1))
            }
            return results
        }
    }

}

trait SyntaxAnalyser extends JavaTokenParsers {

    import OctoTree._

    def indices : Parser[List[String]] =
        "[" ~> rep(ident) <~ "]"

    def tensor : Parser[Tensor] =
        ident ~ indices ^^ {case n ~ i => Tensor(n,i.toSet)}

    // Top Level Program = a single sum (for now)
    def program : Parser[Program] =
        tensor ~ "+=" ~ rep(tensor) ^^
            {case lhs ~ "+=" ~ rhs => Program(lhs, rhs)}
}

object MPrinter extends PrettyPrinter {
    import OctoTree._

    def pretty(t : Exp) : String = {
        super.pretty(show(t))
    }

    // returns the current expression document and the statement document
    def flatten(e: Exp) : (Doc, Doc)  = e match {
        case Tensor(n,i) => (show(Tensor(n,i)), empty)
        case Mul(res, lhs, rhs) => {
            val subl = flatten(lhs)
            val subr = flatten(rhs)
            // ("(" <> subl._1 <> "*" <> subr._1 <> ")", subl._2 <> subr._2)
            // removing parens for now
            (subl._1 <> "*" <> subr._1, subl._2 <> subr._2)
        }
        case Sum(res, ind, rhs) => {
            val sub = flatten(rhs)
            (show(res), sub._2 <> "\t" <> show(res) <+> "+=" <+> sub._1 <> line)
        }

    }

    def show(t : Exp) : Doc = t match {
        case Tensor(name,ind) => name <> ":(" <> ssep(ind.toList.map(text),",") <> ")"
        case Mul(res, lhs, rhs) => flatten(Mul(res, lhs, rhs))._2
        case Sum(res, ind, rhs) => flatten(Sum(res, ind, rhs))._2
    }
}

object Flat extends PrettyPrinter {
    import OctoTree._

    def pretty(t : Exp) : String = {
        super.pretty(show(t))
    }

    def maybeAssign(e : Exp, curexpr : Doc) : (Doc,Doc)  = e match {
        case Tensor(n,i) => (curexpr, empty)
        case Mul(o,l,r) => (curexpr, empty)
        case Sum(res, ind, rhs) => (show(res), "\t" <> show(res) <+> "=" <+> curexpr <> line)
    }

    // returns the current expression document and the statement document
    def flatten(e : Exp) : (Doc, Doc) = e match {
        case Tensor(name,ind) => (show(Tensor(name,ind)),empty)
        case Sum(res, ind, rhs) =>  {
            val sub = flatten(rhs)
            ("Sum(" <> ind <> "," <+> sub._1 <> ")", sub._2)
        }
        case Mul(res, lhs, rhs) => {
            //println("entering mul")
            //println(lhs)
            //println(rhs)
            val subl = flatten(lhs)
            val subr = flatten(rhs)
            val maybeL = maybeAssign(lhs, subl._1)
            val maybeR = maybeAssign(rhs, subr._1)
            //println(super.pretty(maybeL._2))
            //println(super.pretty(maybeR._2))
            ("(" <> maybeL._1 <> "*" <> maybeR._1 <> ")", subl._2 <> subr._2 <> maybeL._2 <> maybeR._2)
        }
    }


    def show(t : Exp) : Doc = t match {
        case Tensor(name,ind) => name <> ":(" <> ssep(ind.toList.map(text),",") <> ")"
        // should be res =
        case Mul(res, lhs, rhs) => {
            val meh = flatten(Mul(res, lhs, rhs))
            "\t" <> meh._2 <> show(res) <+> "=" <+> meh._1
        }
        case Sum(res, ind, rhs) =>  {
            val meh = flatten(Sum(res, ind, rhs))
            meh._2 <> "\t" <> show(res) <+> "=" <+> meh._1
        }
    }


}

object Prefix extends PrettyPrinter {
    import OctoTree._

    def pretty(t: Exp) : String = super.pretty(show(t))

    def show(e : Exp) : Doc = e match {
        case Tensor(name, ind) => text(name)
        case Mul(res, lhs, rhs) => "*" <+> show(lhs) <+> show(rhs)
        case Sum(res, ind, rhs) => "+" <> text(ind) <+> show(rhs)
    }
}

object Explicit extends PrettyPrinter {
    import OctoTree._

    def pretty(t: Exp) : String = super.pretty(show(t))

    def show(e : Exp) : Doc = e match {
        case Tensor(name,ind) => name <> "[" <> sep(ind.toList.map(text)) <> "]"
        case Mul(res, lhs, rhs) => "Mul(" <> show(res) <> "," <+> show(lhs) <+> "," <+> show(rhs) <> ")"
        case Sum(res, ind, rhs) => "Sum(" <> show(res) <> "," <+> text(ind) <+> "," <+> show(rhs) <> ")"
    }
}

object GraphViz extends PrettyPrinter {
    import OctoTree._

    def pretty(t: Exp) : String = super.pretty(show(t))

    def tns(t : Tensor) : Doc = t match {
        case Tensor(name,ind) => name <> "[" <> sep(ind.toList.map(text)) <> "]"
    }

    def show(e : Exp) : Doc = e match {
        case Tensor(name,ind) => name <> "[label = \"" <> tns(Tensor(name,ind)) <> "\"];" <> line
        case Mul(res, lhs, rhs) => res.name <> "[label = \"" <> tns(res) <> "\"];" <@>
            res.name <+> "->" <+> lval(lhs).name <> "[color=\"black\"];" <@>
            res.name <+> "->" <+> lval(rhs).name <> "[color=\"black\"];" <@>
            show(lhs) <> show(rhs)

        case Sum(res, ind, rhs) => res.name <> "[label = \"" <> tns(res) <> "\"];" <@>
            res.name <+> "->" <+> lval(rhs).name <> "[color=\"black\", label=" + ind + "];" <@>
            show(rhs)
    }
}


/*
trait Evaluator {
    import OctoTree._
    import org.kiama.rewriting.Rewriter._

    val tensor_rewrite = // should be (ONode => Option[ONode]) , but can't typecheck
        rule[ONode] {
            case Tensor(name, ind) => IndTensor(name, ind.head)
        }    <+ id // in all other cases we do nothing and pass

        //    <+ (id:(ONode => Option[ONode]))  this cast doesn't work, can't typecheck the strategies


    val change_all_tensors = topdown(tensor_rewrite)

}
*/

/*
trait Evaluator {
    import OctoTree._

    def express(s: Sum) : Code = {
        s match {
            case Sum(decl, lhs, ind, rhs) =>
                Code(decl, ind ++ lhs.ind, lowerLoops(ind ++ lhs.ind, lhs, rhs))
            // case _ => throw new RuntimeException("unrecognized case in express")
        }
    }

    def lowerLoops(indexList : List[String], lhs: Tensor, rhs: Exp) : Stmt = {
        indexList match {
            case List() => Assign(lhs, rhs)
            case x :: xs => Loop(lowerLoops(xs, lhs, rhs), x, "int " + x + " = 0; " + x + " < 10; ++" + x)
        }
    }
}
*/

// object OCTOPI extends SyntaxAnalyser with Evaluator {
object OCTOPI extends SyntaxAnalyser {

    import OctoTree._
    import org.kiama.rewriting.Rewriter.rewrite

    def main(args: Array[String]) {

        import java.io.FileReader
        import java.io.PrintWriter
        import java.io.File

        val reader = new FileReader(args(0))

        val parser_result = parseAll(program, reader)

        val base_name = args(0).slice(args(0).lastIndexOf("/") + 1,args(0).size - 2)
        val dir_name = args(0).slice(0,args(0).size-2) + "_tmp/"

        val dir:File = new File(dir_name)


        val success = dir.mkdir()

        if (success) {
            println("Directory created")
        } else {
            println("Directory already exists, overwriting files")
        }

        parser_result match {
            case Success(program,i) => {
                // println(program.rhs)
                val options = implement(0,program.lhs.ind, List[Exp](), program.rhs)
                for (o <- options.zip(0 to options.size)) {
                    val output = o._1
                    val renamed = output match {
                        case Tensor(n,i) => program.lhs
                        case Mul(res, lhs, rhs) => Mul(program.lhs, lhs, rhs)
                        case Sum(res, siv, rhs) => Sum(program.lhs, siv, rhs)
                    }

                    val indexSet:Set[String] = (program.lhs.ind /: program.rhs)({ case (vals, t) => vals.union(t.ind)})
                    val i = o._2

                    // Shorthand version
                    // println(i + ": " + Prefix.pretty(renamed))
                    // println(i + ": " + Explicit.pretty(renamed))

                    val vars = tensorTemps(renamed)
                    val z = new File(dir_name + base_name + "_" + i + ".m")
                    val wr = new PrintWriter(dir_name + base_name + "_" + i + ".m")
                    wr.println(base_name)
                    wr.println("access: linearize")
                    wr.println("define:")
                    for (i <- indexSet) {
                        wr.println("\t" + i.toUpperCase + " = 10")
                    }
                    wr.println("variables:")
                    for (t <- vars) {
                        wr.println("\t" + varDecl(t))
                    }
                    wr.println("operations:")
                    wr.println(Flat.pretty(renamed))
                    wr.close()

                    // dot file
                    /*
                    val gdot = new File(dir_name + base_name + "_" + i + ".dot")
                    val gw = new PrintWriter(dir_name + base_name + "_" + i + ".dot")
                    gw.println("digraph G {")
                    gw.println(GraphViz.pretty(renamed))
                    gw.println("}")
                    gw.close()
                    */
                }
            }
            case Failure(m,i) => println(m)
            case Error(m,i) => println(m)
        }
    }
}
