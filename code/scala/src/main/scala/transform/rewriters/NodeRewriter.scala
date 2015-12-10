package transform.rewriters

import parsers.TParser._


/*trait NodeRewriter[R>:Node,T>:Node]{


  def rewrite(node: R): Option[T]
}*/





object Rewriters {
  trait Rewriter

  type MatchingGroup = Seq[Node]
  type Matcher = Seq[Node] => Option[Match]
  case class Match(consumed: Int, groups: Seq[MatchingGroup])


  val transformations: Map[Matcher,Seq[Rewriter]]= Map.empty //introduce some pritorities  (ifBlock -> Seq(ifRewriter))
/*
  def rewrite(input: Seq[Node]) = {
    for {
      matcher   <- transformations.keys
      matchRes  <- matcher(input)
      rewriter  <-
    }
  }
*/


  //MATCHERI


  //NO NESTED IFS!!!!!
  def ifBlock(input: Seq[Node]): Option[Match] = {
    input match {
      case Func(name,_,_)::tail if name.startsWith("if")=> {
        val elseIndex = tail.indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("else"))
        val fiIndex = tail.drop(elseIndex+1).indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("fi"))
        //TODO bleh, chain options, FOR comprehens...
        if(elseIndex >= 0 && fiIndex >= 0) {
          Some(Match(fiIndex+2,Seq(tail.take(elseIndex),tail.slice(elseIndex+1,elseIndex+fiIndex+1))))
        } else {
         None
        }
      }
      case _ => None
    }
  }




  //REWRITERI
  //
  def remove(input: Seq[Node], matching: Match): Seq[Node] = Seq.empty
  def elseRemoval(input: Seq[Node], matching: Match): Seq[Node] = {
    matching.groups.head //take only elements nested under if clause
  }


}

//func(blockArg bez fArga) stringNode blockArg    <----------komentar slike

//rewriter
//structural patterns


//niz od 3 -> niz od 1
//1 i njegovu strukturu

//pattern(input) => na matchanom consumanom inputu odradi pravila za taj pattern
//pattern = Seq[RawNode]
//BFS descend po djeci, kod flattanog krece matching
  //za svaki node sekve, probaj ga matchat s pocetkom nekog patterna, consumaj dalje, do kraja
  //sacuvaj matchinge i tailove za svaki okinuti
  //odredi redosljed po prioritetima
  //nakon primjene svakoga,
  //provrti ponovo matching