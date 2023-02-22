import java.util
import scala.collection.mutable

// clasa speciala pentru a reprezenta generic o stare
class state[N](var name: N, var outStates: mutable.HashMap[N, String], var isFinal: Boolean, var isInitial: Boolean) {

    override def toString: String = {

        var res:String = new String
        res += "name = " + name
        if (outStates != null) {
            res += " outStates: " + outStates.toString()
        } else {
            res += " outStates: null "
        }
        if (isFinal) {
            res += " FINAL"
        } else if (isInitial) {
            res += " INITIAL"
        }
        res += "\n"
        return res
    }
}


class Nfa[A](/* TODO : define the constructor params */ var automate: util.ArrayList[state[A]]) {

    // The following methods are only the methods directly called by the test suite. You can (and should) define more.

    def map[B](f: A => B) : Nfa[B] = {

        val a_new: util.ArrayList[state[B]] = new util.ArrayList[state[B]]()
        for(i <- 0 until this.automate.size){ // pentru fiecare stare din nfa
            // creez o noua stare care are tipul cerut
            val s:state[B] = new state(f(automate.get(i).name), new mutable.HashMap[B, String], automate.get(i).isFinal, automate.get(i).isInitial)
            if(automate.get(i).outStates.nonEmpty) { // pentru fiecare stare la care era conectata
                for ((k, v) <- automate.get(i).outStates) {
                    s.outStates.addOne(f(k), v) // transform elementele din map-ul de conexiuni
                }
            }
            a_new.add(s)
        }
        return new Nfa[B](a_new)
    }// TODO implement map

    def next(state: A, c: Char): Set[A] = {
        val mySet: Set[A] = Set()
        for((k, v) <- state.asInstanceOf[state[A]].outStates){
            for(i <- 0 until automate.size()){
                if(k == automate.get(i).name){
                    mySet + automate.get(i).asInstanceOf[A]
                }
            }
        }
        return mySet
    } // TODO implement next

    def accepts(str: String): Boolean = {

        // aplic o parcurgere DFS a nfa-ul pornind de la starea initiala
        if (DFS(this.getInitial.name, 0, str)) {
            return true
        } else {
            return false
        }
        return false
    } // TODO implement accepts

    def DFS(state: A, index: Int, str: String): Boolean = {

        if(index > str.length){
            return false
        }

        for (i <- 0 until automate.size) {
            if(automate.get(i).name == state){
                if(automate.get(i).isFinal){
                    if(index == str.length) {
                        return true
                    }
                }
                if(automate.get(i).outStates != null) {
                    for((k, v) <- automate.get(i).outStates){
                        if(v == "eps"){
                            if (DFS(k, index, str)) {
                                return true
                            }
                        }
                        if(index < str.length && v == str.charAt(index).toString){
                            if(DFS(k, index + 1, str)){
                                return true
                            }
                        }
                    }
                }
            }
        }
        return false
    }

    def getStates : Set[A] = {

        val mySet: Set[A] = Set()
        for(i <- 0 until automate.size){
            mySet + automate.get(i).asInstanceOf[A]
        }
        return mySet
    }// TODO implement getStates

    def isFinal(state: A): Boolean = {
        if(state.asInstanceOf[state[A]].isFinal){
            return true
        } else {
            return false
        }
    }  // TODO implement isFinal

    def getInitial: state[A] = {
        for (i <- 0 until automate.size) {
            if (automate.get(i).isInitial) {
                return automate.get(i)
            }
        }
        return null
    }

    def getState(name: A): state[A] = {
        for (i <- 0 until automate.size) {
            if (automate.get(i).name == name) {
                return automate.get(i)
            }
        }
        return null
    }

    override def toString: String = {

        var res:String = new String()
            for (i <- 0 until automate.size) {
                res += "name = " + automate.get(i).name
                if (automate.get(i).outStates != null) {
                    res += " outStates: " + automate.get(i).outStates.toString()
                } else {
                    res += " outStates: null "
                }
                if(automate.get(i).isFinal){
                    res += " FINAL"
                }
                if(automate.get(i).isInitial){
                    res += " INITIAL"
                }
                res += "\n"
        }
        return res
    }
}

object Nfa {

    var last: mutable.Stack[String] = new mutable.Stack[String] // pentru a tine ordinea ultimelor doua entitati

    // functiile construiesc o lista de stari care reprezinta un nfa
    def concat(atomStack: mutable.Stack[String], NfaStack: mutable.Stack[util.ArrayList[state[Int]]], counter: Int): Int = {

        val miniNfa: util.ArrayList[state[Int]] = new util.ArrayList[state[Int]]
        var stateCount: Int = counter
        var f: String = last.pop()
        var s: String = last.pop()

        if (atomStack.size >= 2 && f == "atom" && s == "atom") {

            val first: String = atomStack.pop()
            val second: String = atomStack.pop()

            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> first), false, true))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps"), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> second), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
            stateCount += 1
            NfaStack.push(miniNfa)

        } else if (atomStack.nonEmpty && NfaStack.nonEmpty && f != s) {

            val first: String = atomStack.pop()
            val second: util.ArrayList[state[Int]] = NfaStack.pop()

            if(f == "atom") {
                miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> first), false, true))
                stateCount += 1
                for (i <- 0 until second.size) {
                    if (second.get(i).isInitial) {
                        second.get(i).isInitial = false
                        val state: Int = second.get(i).name
                        miniNfa.add(new state(stateCount, mutable.HashMap(state -> "eps"), false, false))
                    }
                }
            }else {
                for (i <- 0 until second.size) {
                    if (second.get(i).isFinal) {
                        second.get(i).isFinal = false
                        second.get(i).outStates = mutable.HashMap(stateCount + 1 -> "eps")
                        stateCount += 1
                    }
                }
                miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> first), false, false))
                stateCount += 1
                miniNfa.add(new state(stateCount, mutable.HashMap(), true, false))
            }
            stateCount += 1
            miniNfa.addAll(second)
            NfaStack.push(miniNfa)

        } else if (NfaStack.size >= 2 && f == "nfa" && s == "nfa") {
            val first: util.ArrayList[state[Int]] = NfaStack.pop()
            val second: util.ArrayList[state[Int]] = NfaStack.pop()
            for (i <- 0 until first.size) {
                if (first.get(i).isFinal) {
                    first.get(i).isFinal = false
                    for (j <- 0 until second.size) {
                        if (second.get(j).isInitial) {
                            second.get(j).isInitial = false
                            first.get(i).outStates = mutable.HashMap(second.get(j).name -> "eps")
                        }
                    }
                }
            }
            miniNfa.addAll(first)
            miniNfa.addAll(second)
            NfaStack.push(miniNfa)
        } else {
            println("Incorrect input")
            return -1
        }
        last.push("nfa")
        return stateCount

    }

    def union(atomStack: mutable.Stack[String], NfaStack: mutable.Stack[util.ArrayList[state[Int]]], counter: Int): Int = {

        val miniNfa: util.ArrayList[state[Int]] = new util.ArrayList[state[Int]]
        var stateCount: Int = counter
        var f: String = last.pop()
        var s: String = last.pop()

        if (atomStack.size >= 2 && (f == "atom") && (s == "atom")) {

            val first: String = atomStack.pop()
            val second: String = atomStack.pop()

            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps", stateCount + 2 -> "eps"), false, true))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 2 -> first), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 2 -> second), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 2 -> "eps"), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps"), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
            stateCount += 1
            NfaStack.push(miniNfa)
        } else if (atomStack.nonEmpty && NfaStack.nonEmpty && f != s) {

            val first: String = atomStack.pop()
            val second: util.ArrayList[state[Int]] = NfaStack.pop()

            for (i <- 0 until second.size) {
                if (second.get(i).isInitial) {
                    second.get(i).isInitial = false
                    miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps", second.get(i).name -> "eps"), false, true))
                }
            }
            stateCount += 1

            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> first), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps"), false, false))
            stateCount += 1

            for (i <- 0 until second.size) {
                if (second.get(i).isFinal) {
                    second.get(i).isFinal = false
                    miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
                    second.get(i).outStates = mutable.HashMap(stateCount -> "eps")
                    stateCount += 1
                }
            }


            miniNfa.addAll(second)
            NfaStack.push(miniNfa)
        } else if (NfaStack.size >= 2 && f == "nfa" && s == "nfa") {

            val first: util.ArrayList[state[Int]] = NfaStack.pop()
            val second: util.ArrayList[state[Int]] = NfaStack.pop()

            for (i <- 0 until first.size) {
                if (first.get(i).isInitial) {
                    first.get(i).isInitial = false
                    for (j <- 0 until second.size) {
                        if (second.get(j).isInitial) {
                            second.get(j).isInitial = false
                            miniNfa.add(new state(stateCount, mutable.HashMap(first.get(i).name -> "eps", second.get(j).name -> "eps"), false, true))
                        }
                    }
                }
            }
            stateCount += 1

            for (i <- 0 until first.size) {
                if (first.get(i).isFinal) {
                    first.get(i).isFinal = false
                    for (j <- 0 until second.size) {
                        if (second.get(j).isFinal) {
                            second.get(j).isFinal = false
                            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
                            first.get(i).outStates = mutable.HashMap(stateCount -> "eps")
                            second.get(j).outStates = mutable.HashMap(stateCount -> "eps")
                        }
                    }
                }
            }
            stateCount += 1

            miniNfa.addAll(first)
            miniNfa.addAll(second)
            NfaStack.push(miniNfa)

        } else {
            println("Incorrect input")
            return -1
        }
        last.push("nfa")
        return stateCount


    }

    def star(atomStack: mutable.Stack[String], NfaStack: mutable.Stack[util.ArrayList[state[Int]]], counter: Int): Int = {

        val miniNfa: util.ArrayList[state[Int]] = new util.ArrayList[state[Int]]
        var stateCount: Int = counter
        var f: String = last.pop()

        if (atomStack.nonEmpty && f == "atom") {

            val first: String = atomStack.pop()

            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps", stateCount + 3 -> "eps"), false, true))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> first), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount - 1 -> "eps", stateCount + 1 -> "eps"), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
            stateCount += 1
            NfaStack.push(miniNfa)
            last.push("nfa")

        } else if (NfaStack.nonEmpty && f == "nfa") {

            val first: util.ArrayList[state[Int]] = NfaStack.pop()
            for (i <- 0 until first.size) {
                if (first.get(i).isInitial) {
                    first.get(i).isInitial = false
                    for (j <- 0 until first.size) {
                        if (first.get(j).isFinal) {
                            first.get(j).isFinal = false
                            miniNfa.add(new state(stateCount, mutable.HashMap(first.get(i).name -> "eps", stateCount + 1 -> "eps"), false, true))
                            stateCount += 1
                            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
                            first.get(j).outStates = mutable.HashMap(stateCount -> "eps", first.get(i).name -> "eps")
                            stateCount += 1
                        }
                    }
                }
            }
            miniNfa.addAll(first)
            NfaStack.push(miniNfa)
            last.push("nfa")

        } else {
            println("Incorrect input")
            return -1
        }
        return stateCount

    }

    def plus(atomStack: mutable.Stack[String], NfaStack: mutable.Stack[util.ArrayList[state[Int]]], counter: Int): Int = {

        val miniNfa: util.ArrayList[state[Int]] = new util.ArrayList[state[Int]]
        var stateCount: Int = counter

        if (atomStack.nonEmpty) {

            val first: String = atomStack.pop()

            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> "eps"), false, true))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount + 1 -> first), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap(stateCount - 1 -> "eps", stateCount + 1 -> "eps"), false, false))
            stateCount += 1
            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
            stateCount += 1
            NfaStack.push(miniNfa)

        } else if (NfaStack.nonEmpty) {

            val first: util.ArrayList[state[Int]] = NfaStack.pop()

            for (i <- 0 until first.size) {
                if (first.get(i).isInitial) {
                    first.get(i).isInitial = false
                    for (j <- 0 until first.size) {
                        if (first.get(j).isFinal) {
                            first.get(j).isFinal = false
                            miniNfa.add(new state(stateCount, mutable.HashMap(first.get(i).name -> "eps"), false, true))
                            stateCount += 1
                            miniNfa.add(new state(stateCount, mutable.HashMap[Int, String](), true, false))
                            first.get(j).outStates = mutable.HashMap(stateCount -> "eps", first.get(i).name -> "eps")
                            stateCount += 1
                        }
                    }
                }
            }
            miniNfa.addAll(first)
            NfaStack.push(miniNfa)

        } else {
            println("Incorrect input")
            return -1
        }
        last.push("nfa")
        return stateCount


    }

    def fromPrenex(str: String): Nfa[Int] = {

        val stari: util.ArrayList[state[Int]] = new util.ArrayList[state[Int]]()

        if (str == "void") {
            stari.add(new state(0, mutable.HashMap[Int, String](), false, true))
            return new Nfa(stari)
        }

        if (str == "eps") {
            stari.add(new state(0, mutable.HashMap[Int, String](), true, true))
            return new Nfa(stari)
        }
        // cazul in care este un nfa cu un singur char
        if (str.length == 1) {
            stari.add(new state(0, mutable.HashMap(1 -> str), false, true))
            stari.add(new state(1, mutable.HashMap[Int, String](), true, false))
            return new Nfa(stari)
        }
        // cazul pentru caractere speciale
        if (str.charAt(0) == '\'') {
            val prelucrare_string: String = str.substring(1, str.length - 1)
            stari.add(new state(0, mutable.HashMap(1 -> prelucrare_string), false, true))
            stari.add(new state(1, mutable.HashMap[Int, String](), true, false))
            return new Nfa(stari)
        }

        var res: Array[String] = str.split(' ')
        res = res.reverse

        var stateCount: Int = 0 // index pentru numele starilor
        val atomStack: mutable.Stack[String] = mutable.Stack[String]() // stack pentru atomi
        // stack pentru starile care se genereaza pe parcursul citirii inputului
        val NfaStack: mutable.Stack[util.ArrayList[state[Int]]] = mutable.Stack[util.ArrayList[state[Int]]]()

        for (elem <- res) {

            if (elem == "CONCAT") {
                stateCount = concat(atomStack, NfaStack, stateCount)
            }
            if (elem == "UNION") {
                stateCount = union(atomStack, NfaStack, stateCount)
            }
            if (elem == "STAR") {
                stateCount = star(atomStack, NfaStack, stateCount)
            }
            if (elem == "MAYBE") {

                atomStack.push("eps")
                last.push("atom")
                stateCount = union(atomStack, NfaStack, stateCount)
            }
            if (elem == "PLUS") {

                stateCount = plus(atomStack, NfaStack, stateCount)
            }

            if (elem.length == 1 || elem == "eps") {
                if (elem.charAt(0).isLetter || elem.charAt(0).isDigit) {
                    atomStack.push(elem)
                    last.push("atom")
                }
            }

        }
        return new Nfa[Int](NfaStack.top)
    } // TODO implement Prenex -> Nfa transformation.

}// You can add more methods to this object
