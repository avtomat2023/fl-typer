package unification

import fltype._
import scala.collection.mutable

case class TypeEquation(lhs: FLType, rhs: FLType) {
  def vars: Set[TypeVar] = lhs.vars union rhs.vars
  def subst(variable: TypeVar, flType: FLType): TypeEquation =
    TypeEquation(lhs.subst(variable, flType), rhs.subst(variable, flType))

  def isRecursive: Boolean = (lhs, rhs) match {
    case (TypeVar(_,_), Application(ts)) => ts.contains(lhs)
    case _ => false
  }

  def isDifferentApplication: Boolean = (lhs, rhs) match {
    case (Application(_), Application(_)) => lhs.kind != rhs.kind
    case _ => false
  }

  def isTrivial: Boolean = (lhs, rhs) match {
    case (TypeVar(_,_), TypeVar(_,_)) => lhs == rhs
    case _ => false
  }

  def decomposeOption: Option[Seq[TypeEquation]] = (lhs, rhs) match {
    case (Application(lhsTypes), Application(rhsTypes)) =>
      if (lhs.kind == rhs.kind)
        Some((lhsTypes zip rhsTypes).map{
          case (lhs, rhs) => TypeEquation(lhs, rhs)
        })
      else
        None
    case _ => None
  }

  def isFlippable: Boolean = (lhs, rhs) match {
    case (Application(_), TypeVar(_,_)) => true
    case _ => false
  }

  def toVisual: visual.Equation = visual.Equation(lhs.toVisual, rhs.toVisual)
}

sealed trait Result {
  def toVisual: visual.Richtext
}
case class Success(result: FLType) extends Result {
  def toVisual = SharpTypeVarGenerator.sharpen(result).toVisual
}
case class Error(msg: String) extends Result {
  def toVisual = visual.Richtext("ill-typed: " + msg)
}

case class SharpTypeVarGenerator(varCount: Int) {
  import SharpTypeVarGenerator.greeks
  def next: (TypeVar, SharpTypeVarGenerator) = {
    val n = greeks.length
    if (varCount < n)
      (TypeVar(greeks(varCount)), SharpTypeVarGenerator(varCount + 1))
    else
      (TypeVar(greeks(varCount % n), varCount / n),
       SharpTypeVarGenerator(varCount + 1))
  }
}

object SharpTypeVarGenerator {
  private val greeks = IndexedSeq(
    "α", "β", "γ", "δ", "ε", "ζ", "η", "θ",
    "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π",
    "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"
  )

  // 型変数の名前を付け替えて見やすくする
  // 例 sharpenTypeVars(TypeVar("τ", 11) --> TypeVar("τ", 3))
  //    ==> TypeVar("α") --> TypeVar("β")
  def sharpen(flType: FLType): FLType = {
    def go(flType: FLType,
           mapping: Map[Int, TypeVar],
           generator: SharpTypeVarGenerator):
        (FLType, Map[Int, TypeVar], SharpTypeVarGenerator) = flType match {
      case TypeVar(greek, numOpt) => {
        require(greek == "τ")
        require(!numOpt.isEmpty)
        val n = numOpt.get
        mapping.get(n) match {
          case Some(v) => (v, mapping, generator)
          case None => {
            val (newVar, newGenerator) = generator.next
            (newVar, mapping + (n -> newVar), newGenerator)
          }
        }
      }
      case FLInt => (FLInt, mapping, generator)
      case FLBool => (FLBool, mapping, generator)
      case FLList(paramType) => {
        val (newParamType, newMapping, newGenerator) =
          go(paramType, mapping, generator)
        (FLList(newParamType), newMapping, newGenerator)
      }
      case argType ->: retType => {
        val (t1, m1, g1) = go(argType, mapping, generator)
        val (t2, m2, g2) = go(retType, m1, g1)
        (t1 ->: t2, m2, g2)
      }
    }
    go(flType, Map(), SharpTypeVarGenerator(0))._1
  }
}

// 相当効率が悪い
// 線形時間にするには、nextが継続を返すようにすれば良い
case class Unification(val eqs: mutable.ListBuffer[TypeEquation],
                       val solveFor: TypeVar = TypeVar("τ")) {
  import Unification._

  def isAbsurd: Boolean =
    eqs.exists(_.isDifferentApplication) || eqs.exists(_.isRecursive)

  def removeTrivial(): Boolean = {
    val i = eqs.indexWhere(_.isTrivial)
    if (i != -1) {
      eqs.remove(i)
      true
    } else false
  }

  @annotation.tailrec
  final def findDecomposable(from: Int = 0): Option[(Int, Seq[TypeEquation])] = {
    if (from >= eqs.length) None
    else eqs(from).decomposeOption match {
      case Some(decomposed) => Some((from, decomposed))
      case None => findDecomposable(from + 1)
    }
  }
  def decomposeApplication(): Boolean = findDecomposable() match {
    case Some((index, decomposed)) => {
      eqs.remove(index)
      eqs ++= decomposed
      true
    }
    case None => false
  }

  def flipApplicationAndVar(): Boolean = {
    val i = eqs.indexWhere(_.isFlippable)
    if (i != -1) {
      val eq = eqs(i)
      eqs.remove(i)
      eqs += TypeEquation(eq.rhs, eq.lhs)
      true
    } else false
  }

  @annotation.tailrec
  final def substitute(from: Int = 0): Boolean = {
    if (from >= eqs.length) false
    else {
      val eq = eqs(from)
      val (lhs, rhs) = (eq.lhs, eq.rhs)
      (lhs, rhs) match {
        case (TypeVar(t,n), _) => {
          if (!eq.isTrivial && !eq.isRecursive) {
            val others = eqs.view(0, from) ++ eqs.view(from+1, eqs.length)
            if (others.exists(_.vars.contains(TypeVar(t,n)))) {
              for (i <- Range(0, eqs.length).filter(_ != from))
                eqs(i) = eqs(i).subst(TypeVar(t,n), rhs)
              true
            } else substitute(from + 1)
          } else substitute(from + 1)
        }
        case _ => substitute(from + 1)
      }
    }
  }

  def next(): Option[Result] = {
    if (isAbsurd) Some(Error("absurdity"))
    else if (removeTrivial() ||
             decomposeApplication() ||
             flipApplicationAndVar() ||
             substitute()) None
    else eqs.find(_.lhs == solveFor) match {
      case Some(eq) => Some(Success(eq.rhs))
      case None => Some(Error("could not solve"))
    }
  }

  @annotation.tailrec
  final def solve(): Result = next() match {
    case Some(result) => result
    case None => solve()
  }

  def toVisual: visual.Equations = visual.Equations(eqs.map(_.toVisual))
}
