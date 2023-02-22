import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import java.util

class ast(var root: String, var left: ast, var right: ast){

    override def toString: String = root + ": " + left.root + " -- " + right.root + "\n"

}


object Regex {

    var prenex: String = new String

    def preprocess(s: List[Char]): String = {

        var i: Int = 0
        var res: String = new String

        while(i < s.size){


            if(s.apply(i) == '['){
                var start: Int = s.apply(i+1).toInt
                var end: Int = s.apply(i+3).toInt
                res += '('
                res += start.toChar
                while(start < end){
                    res += '|'
                    start += 1
                    res += start.toChar
                }
                res += ')'
                i += 4
            }else if(s.apply(i) == '\''){
                res += s.apply(i+1)
                i += 2
            } else {
                res += s.apply(i).toString
            }
            i += 1
        }
        return res

    }

    // generez prenexul folosind o parcurgere preordine
    def RSD(node: ast): Any = {


        if (node != null) {
            if (node.root == ".") {
                prenex += " CONCAT"
            } else if (node.root == "|") {
                prenex += " UNION"
            } else if (node.root == "*") {
                prenex += " STAR"
            } else if (node.root == "+") {
                prenex += " PLUS"
            } else if (node.root == "?") {
                prenex += " MAYBE"
            }else
                    prenex += " " + node.root
            }
        if (node.left != null) {
            RSD(node.left)
        }
        if (node.right != null) {
            RSD(node.right)
        }
    }

    def buildAtomAst(root: String, right: String, left: String): ast = {

        val node: ast = new ast(root, null, null)
        if(right != null) {
            node.right = new ast(right, null, null)
        }
        if(left != null) {
            node.left = new ast(left, null, null)
        }
        return node
    }

    def buildCombinedAst(root: String, miniAst: ast, atom: String, atomFirst: Boolean): ast = {

        // cazul in care sunt doua concatenari consecutive
        // si evit generarea unei secvente de genul: C C C C x y z..
        if(miniAst.root == "." && root == "."){
            var operator: String = miniAst.root
            var aux: ast = miniAst
            var prev: ast = miniAst
            while(operator == "."){
                operator = aux.right.root
                prev = aux
                aux = aux.right
            }

            val node: ast = new ast(root, null, null)
            node.left = aux
            node.right = new ast(atom, null, null)
            prev.right = node
            return miniAst
        } else {
            val node: ast = new ast(root, null, null)
            if (atomFirst) {
                node.right = new ast(atom, null, null)
                node.left = miniAst
            } else {
                node.right = miniAst
                node.left = new ast(atom, null, null)
            }
            return node
        }
    }


    def buildAst(root: String, right: ast, left: ast): ast = {

        // cazul in care sunt doua concatenari consecutive
        // si evit generarea unei secvente de genul: C C C C x y z..
        if(left.root == "." && root == ".") {
            var operator: String = left.root
            var aux: ast = left
            var prev: ast = left
            while (operator == ".") {
                operator = aux.right.root
                prev = aux
                aux = aux.right
            }

            val node: ast = new ast(root, null, null)
            node.left = aux
            node.right = right
            prev.right = node
            return left

        }else {
            return new ast(root, left, right)
        }
    }

    // This function should construct a prenex expression out of a normal one.
    def toPrenex(str: String): String = {

        val p_str = preprocess(str.toList)

        var atomQueue: Queue[String] = new Queue[String]
        var operatorsStack: Stack[String] = new Stack[String]
        var op_priority: util.HashMap[String, Int] = new util.HashMap[String, Int]
        op_priority.put(")", 5)
        op_priority.put("*", 4)
        op_priority.put("+", 4)
        op_priority.put("?", 3)
        op_priority.put(".", 2)
        op_priority.put("|", 1)
        op_priority.put("(", 0)
        var isConcat: Boolean = false

        if(p_str == "eps"){
            return "eps"
        }
        if(p_str.length == 1){
            return p_str
        }

        // generez forma polish din regexul dat
        for(elem <- p_str){
            if(elem == '|'){
                isConcat = false
                if (operatorsStack.nonEmpty) {
                    if (op_priority.get(elem.toString) >= op_priority.get(operatorsStack.top)) {
                        operatorsStack.push(elem.toString)
                    } else {
                        while (operatorsStack.nonEmpty && op_priority.get(elem.toString) <= op_priority.get(operatorsStack.top)) {
                            atomQueue.addOne(operatorsStack.pop())
                        }
                        operatorsStack.push(elem.toString)
                    }
                } else {
                    operatorsStack.push(elem.toString)
                }

            }else if(elem == '*' || elem == '+' || elem == '?'){
                isConcat = true
                if(operatorsStack.nonEmpty) {
                    if (op_priority.get(elem.toString) >= op_priority.get(operatorsStack.top)) {
                        operatorsStack.push(elem.toString)
                    } else {
                        while (operatorsStack.nonEmpty && op_priority.get(elem) <= op_priority.get(operatorsStack.top)) {
                            atomQueue.addOne(operatorsStack.pop())
                        }
                        operatorsStack.push(elem.toString)
                    }
                } else{
                    operatorsStack.push(elem.toString)
                }

            }else if(elem == '('){
                if (isConcat) {
                    if (operatorsStack.nonEmpty) {
                        if (op_priority.get('.') >= op_priority.get(operatorsStack.top)) {
                            operatorsStack.push(".")
                        } else {
                            while (operatorsStack.nonEmpty && op_priority.get(".") <= op_priority.get(operatorsStack.top)) {
                                atomQueue.addOne(operatorsStack.pop())
                            }
                            operatorsStack.push(".")
                        }
                    } else {
                        operatorsStack.push(".")
                    }
                }
                operatorsStack.push(elem.toString)
                isConcat = false

            }else if(elem == ')'){
                isConcat = true
                while(operatorsStack.top != "("){
                    atomQueue.addOne(operatorsStack.pop())
                }
                operatorsStack.pop() // scot (
            }else{
                if (isConcat) {
                    // concat
                    if (operatorsStack.nonEmpty) {
                        if (op_priority.get('.') >= op_priority.get(operatorsStack.top)) {
                            operatorsStack.push(".")
                        } else {
                            while (operatorsStack.nonEmpty && op_priority.get(".") <= op_priority.get(operatorsStack.top)) {
                                atomQueue.addOne(operatorsStack.pop())
                            }
                            operatorsStack.push(".")
                        }
                    } else {
                        operatorsStack.push(".")
                    }
                }
                atomQueue.addOne(elem.toString)
                isConcat = true
            }
        }
        while(operatorsStack.nonEmpty){
            atomQueue.addOne(operatorsStack.pop())
        }


        val atomStack: Stack[String] = new Stack[String]
        val astStack: Stack[ast] = new Stack[ast]
        var atomLast: Boolean = false // retin daca ultimul element de adaugat in AST este un atom
                                      // pentru a respecta ordinea adaugarii acestuia

        while(atomQueue.nonEmpty){
            var elem: String = new String
            elem = atomQueue.dequeue()

            if(elem == "|" || elem == ".") {
                // chiar daca am mai multi atomi in stiva verific daca ultimul element de adaugat este un miniAst
                if (atomStack.size >= 2 && atomLast) {
                    // concat 2 atomi
                    astStack.push(buildAtomAst(elem, atomStack.pop(), atomStack.pop()))
                } else if (astStack.size >= 2) {
                    // concat 2 ast-uri
                    astStack.push(buildAst(elem, astStack.pop(), astStack.pop()))
                } else {
                    // concat combinat
                    astStack.push(buildCombinedAst(elem, astStack.pop(), atomStack.pop(), atomLast))
                }
                atomLast = false
            }else if (elem == "*" || elem == "+" || elem == "?"){ // nfa.scala poate genera nfa-uri si cu +, ?
                if(atomLast){
                    astStack.push(buildAtomAst(elem, null, atomStack.pop()))
                } else if(astStack.nonEmpty){
                    astStack.push(buildAst(elem, null, astStack.pop()))
                }
                atomLast = false
            }else {
                atomStack.push(elem)
                atomLast = true
            }
        }

        RSD(astStack.pop())
        return prenex
    }
}
