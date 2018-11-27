package com.stremlenye.robotwars.macros

import scala.reflect.macros.blackbox

object FunctorKMacros {
  def derive[Alg[_[_]]](c: blackbox.Context)(implicit tag: c.WeakTypeTag[Alg[Option]]): c.Tree = {
    import c.internal._
    import c.universe._

    val Alg = tag.tpe.typeConstructor
    def abort(msg: String) = c.abort(c.enclosingPosition,
      s"Could not generate a FunctorK instance for type `$Alg` because $msg")

    if (!Alg.typeSymbol.isClass || !Alg.typeSymbol.asClass.isTrait) abort("it is not a trait")
    val f = Alg.typeSymbol.asType.typeParams.head.asType
    val F = f.toType.typeConstructor

    val FAlg = appliedType(Alg, F).dealias
    def alg(ctx: TypeName): Tree = {
      val Fi = FAlg.typeArgs.indexOf(F)
      if (Fi < 0) tq"$FAlg" else {
        val tycon = FAlg.typeConstructor.typeSymbol.asType
        val (prefix, suffix) = FAlg.typeArgs.splitAt(Fi)
        tq"$tycon[..$prefix, $ctx, ..${suffix drop 1}]"
      }
    }

    val G = TypeName(c.freshName("G"))
    val H = TypeName(c.freshName("H"))
    val ga = TermName(c.freshName("ga"))
    val xf = TermName(c.freshName("xf"))

    val methods = for (member <- FAlg.members if member.isAbstract) yield {
      if (!member.isMethod) abort(s"`$member` is not a method")
      val method = member.asMethod
      val methodType = method.typeSignatureIn(FAlg)
      val returnType = methodType.finalResultType

      if (methodType.paramLists.iterator.flatten.exists(_.typeSignature.contains(f)))
        abort(s"`$F` appears as a method parameter")
      if (returnType.typeArgs.exists(_.contains(f)))
        abort(s"`$F` appears as a type argument")

      val tparams = for (t <- methodType.typeParams) yield typeDef(t)
      val targs = for (t <- methodType.typeParams) yield typeRef(NoPrefix, t, Nil)
      val paramss = for (ps <- methodType.paramLists) yield for (p <- ps) yield valDef(p)
      val argss = for (ps <- methodType.paramLists) yield for (p <- ps) yield p.name.toTermName
      val tpt = if (returnType.typeConstructor =:= F) tq"$H[..${returnType.typeArgs}]" else tq"$returnType"
      q"override def ${method.name}[..$tparams](...$paramss): $tpt = $xf($ga.$method[..$targs](...$argss))"
    }

    val GAlg = alg(G)
    val HAlg = alg(H)
    val FunctorK = tq"_root_.cats.tagless.FunctorK"
    val NaturalXf = tq"_root_.cats.arrow.FunctionK"

    q"""new $FunctorK[${polyType(f :: Nil, FAlg)}] {
      def mapK[$G[_], $H[_]]($ga: $GAlg)($xf: $NaturalXf[$G, $H]): $HAlg = new $HAlg {
        ..${methods.toList}
      }
    }"""
  }
}
