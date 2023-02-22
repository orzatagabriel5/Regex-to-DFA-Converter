import scala.collection.mutable
import java.util


class Dfa[A] (/* TODO : define the constructor params */ var automate: util.ArrayList[state[A]]){

    def map[B](f: A => B): Dfa[B] = {

        val a_new: util.ArrayList[state[B]] = new util.ArrayList[state[B]]()
        for (i <- 0 until this.automate.size) { // pentru fiecare stare din nfa
            // creez o noua stare care are tipul cerut
            val s: state[B] = new state(f(automate.get(i).name), new mutable.HashMap[B, String], automate.get(i).isFinal, automate.get(i).isInitial)
            if (automate.get(i).outStates.nonEmpty) { // pentru fiecare stare la care era conectata
                for ((k, v) <- automate.get(i).outStates) {
                    s.outStates.addOne(f(k), v) // transform elementele din map-ul de conexiuni
                }
            }
            a_new.add(s)
        }
        return new Dfa[B](a_new)
    } // TODO implement map

    def next(state: A, c: Char): A = {
        return state.asInstanceOf[state[A]].outStates.keySet.head

    } // TODO implement next

    def accepts(str: String): Boolean = {

        if (!automate.isEmpty) {
            for (i <- 0 until automate.size) {
                if (automate.get(i).isInitial) {
                    if (DFS(automate.get(i).name, 0, str)) {
                        return true
                    } else {
                        return false
                    }
                }
            }
        }
        return false
    } // TODO implement accepts

    def DFS(state: A, index: Int, str: String): Boolean = {

        if (index > str.length) {
            return false
        }
        for (i <- 0 until automate.size) {
            if (automate.get(i).name == state) {

                if (automate.get(i).isFinal) {
                    if (index == str.length) {
                        return true
                    }
                }
                if (automate.get(i).outStates != null) {
                    for ((k, v) <- automate.get(i).outStates) {

                        if (v == "eps") {
                            if (DFS(k, index, str)) {
                                return true
                            }
                        }
                        if (index < str.length && v == str.charAt(index).toString) {
                            if (DFS(k, index + 1, str)) {
                                return true
                            }
                        }
                    }
                }
            }
        }
        return false
    }

    def getStates: Set[A] = {
        val mySet: Set[A] = Set()
        for (i <- 0 until automate.size) {
            mySet + automate.get(i).asInstanceOf[A]
        }
        return mySet
    } // TODO implement getStates

    def isFinal(state: A): Boolean = {
        if (state.asInstanceOf[state[A]].isFinal) {
            return true
        } else {
            return false
        }
    } // TODO implement isFinal

    def getInitial: state[A] = {
        for(i <- 0 until automate.size) {
            if (automate.get(i).isInitial) {
                return automate.get(i)
            }
        }
        return null
    }

    override def toString: String = {

        var res: String = new String()
        for (i <- 0 until automate.size) {
            res += "name = " + automate.get(i).name
            if (automate.get(i).outStates != null) {
                res += " outStates: " + automate.get(i).outStates.toString()
            } else {
                res += " outStates: null "
            }
            if (automate.get(i).isFinal) {
                res += " FINAL"
            } else if (automate.get(i).isInitial) {
                res += " INITIAL"
            }
            res += "\n"
        }
        return res
    }
}

object Dfa {

    def setToInt(x: Set[Int]): Int = {
        var name: Int = 0
        for(nr <- x){
            name = name*10 + nr
        }
        return name
    }

    def getAlphabet(nfa: Nfa[Int]): Set[String] = {

        var res: Set[String] = Set()
        for(i <- 0 until nfa.automate.size){
            if(nfa.automate.get(i).outStates.nonEmpty){
                for( (k,v) <- nfa.automate.get(i).outStates){
                    if(v != "eps") {
                        res += v
                    }
                }
            }
        }
        return res
    }

    def getAlphabetDfa(nfa: Dfa[Int]): Set[String] = {

        var res: Set[String] = Set()
        for (i <- 0 until nfa.automate.size) {
            if (nfa.automate.get(i).outStates.nonEmpty) {
                for ((k, v) <- nfa.automate.get(i).outStates) {
                    if (v != "eps") {
                        res += v
                    }
                }
            }
        }
        return res
    }

    // functie pentru inchiderea starilor din nfa
    def close_state(nfa: Nfa[Int], s: Int): Set[Int] = {
        var res: Set[Int] = Set()
        for( (k,v) <- nfa.getState(s).outStates){
            if(v == "eps"){ // pentru fiecare epsilon-tranzitie
                res += k // consider starea gasita ca accesibila
                res ++= close_state(nfa, k) // si o inchid si aceasta
            }
        }
        return res
    }

    def addSink(dfa: Dfa[Int], alph: Set[String]): Unit = {
        val sink: state[Int] = new state[Int](-1, mutable.HashMap[Int, String](-1 -> "eps"), false, false)
        dfa.automate.add(sink)
        for(i <- 0 until dfa.automate.size){
            val x = dfa.automate.get(i).outStates.values
            for(c <- alph){
                    if(!x.toList.contains(c)){
                        dfa.automate.get(i).outStates.addOne(-1 -> c)
                }
            }
        }
    }

    def fromPrenex(str: String): Dfa[Int] = { // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

        val nfa: Nfa[Int] = Nfa.fromPrenex(str)
        if(str == "eps"){
            return new Dfa[Int](nfa.automate)
        }

        val states_stack: mutable.Stack[Set[Int]] = mutable.Stack[Set[Int]]()
        val miniDfa: util.ArrayList[state[Set[Int]]] = new util.ArrayList[state[Set[Int]]]()
        val alphabet: Set[String] = getAlphabet(nfa) // alfabetul nfa-ului generat
        var alphabet_dfa : Set[Set[Int]] = Set() // multime in care salvez toate starile din dfa

        var initialState: Set[Int] = Set()
        for((_,v) <- nfa.getInitial.outStates){
            if(v != "eps"){
                initialState += nfa.getInitial.name
            }
        }
        if(initialState.isEmpty){ // daca starea initiala are doar tranzitii eps o inchid
            initialState ++= close_state(nfa, nfa.getInitial.name)
        }

        states_stack.addOne(initialState)

        var isInitial: Boolean = true // prima stare prelucrata va fi mereu initiala
        while(states_stack.nonEmpty){
            var aux_state: Set[Int] = states_stack.pop

            if(!alphabet_dfa.contains(aux_state)){ // verific daca dfa-ul contine deja starea pe care vreau sa o prelucrez
                alphabet_dfa += aux_state
                val conections: mutable.HashMap[String, Set[Int]] = new mutable.HashMap[String, Set[Int]]()
                for(c <- alphabet){ // pentru fiecare caracter din alfabet
                    var conected_states: Set[Int] = Set()
                    for(s <- aux_state){ // verific daca starile au tranzitii cu costul c
                        for( (k,v) <- nfa.getState(s).outStates){
                            if(v == c) {    // doar daca o stare are tranzitie cu cost c
                                            // o consider accesibila si o inchid
                                conected_states += k
                                conected_states ++= close_state(nfa, k)
                            }
                        }
                    }
                    if(conected_states.nonEmpty) { // evid adaugarea unui set de stari gol in map
                        conections.addOne(c -> conected_states)
                    }
                }
                var isFinal: Boolean = false
                for(nume <- aux_state){ // verific daca starea din dfa este una finala
                    if(nfa.getState(nume).isFinal){
                        isFinal = true
                    }
                }

                miniDfa.add(new state[Set[Int]](aux_state, mutable.HashMap[Set[Int], String](), isFinal, isInitial))
                isInitial = false

                for((c, stari) <- conections) { // pentru toate caile gasite la fiecare caracter din alfabet
                    var done: Boolean = false
                    for(i <- 0 until miniDfa.size) { // verific daca exista deja un stare in dfa la fel
                        if (stari.equals(miniDfa.get(i).name)) { // caz in care o adaug ca iesire la starea in curs de prelucrare
                            miniDfa.get(miniDfa.size - 1).outStates.addOne(stari -> c)
                            done = true
                        }
                    }
                    if(!done){ // daca nu exista deja, pun calea gasita in stiva
                        miniDfa.get(miniDfa.size - 1).outStates.addOne(stari -> c)
                        states_stack.addOne(stari)
                    }
                }
            }
        }
        // returnez un dfa de tip INT construit din multimea de stari dfa
        val setDfa: Dfa[Set[Int]] = new Dfa[Set[Int]](miniDfa)
        val IntDfa: Dfa[Int] = setDfa.map[Int]((x:Set[Int]) => setToInt(x))
        val alph: Set[String] = getAlphabetDfa(IntDfa)
        addSink(IntDfa, alph)
        return IntDfa
    }
}
