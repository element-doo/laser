package transform.rewriters

import parsers.TParser._

trait NodeRewriter[R>:Node,T>:Node]{


  def rewrite(node: R): Option[T]
}




object Rewriters {
  type MatchingGroup = Seq[Node]
  type Matcher = Seq[Node] => MatchResult
  trait MatchResult
  case class MatchingFailed extends MatchResult
  case class MatchingSuccess(consumed: Int, groups: Seq[MatchingGroup]) extends MatchResult



  /*def tryMatch(input: Seq[Node], matcher: Matcher): MatchResult = {

  }*/


  //NO NESTED IFS!!!!!
  def ifBlock(input: Seq[Node]): MatchResult = {
    input match {
      case Func(name,_,_)::tail if name.startsWith("if")=> {
        val elseIndex = tail.indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("else"))
        val fiIndex = tail.drop(elseIndex+1).indexWhere(node => node.isInstanceOf[Func] && node.asInstanceOf[Func].name.equals("fi"))
        //TODO bleh, chain options, FOR comprehens...
        if(elseIndex >= 0 && fiIndex >= 0) {
          MatchingSuccess(fiIndex+2,Seq(tail.take(elseIndex),tail.slice(elseIndex+1,elseIndex+fiIndex+1)))
        } else {
         MatchingFailed()
        }
      }
      case _ => MatchingFailed()
    }
  }
  
}


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