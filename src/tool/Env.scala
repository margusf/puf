package puf

// Environment for code generation.

/** Type of variable: local or global. */
object VarType extends Enumeration {
    type Type = Value

    val Local, Global = Value
}

class Env(val parent: Env, locals: Map[String, Int],
        globals: Map[String, Int]) {
    /** Return binding of the variable. */
    def apply(id: String): Tuple2[VarType.Type, Int] =
        if (locals.contains(id))
            (VarType.Local, locals(id))
        else if ((globals ne null) && globals.contains(id))
            (VarType.Global, globals(id))
        else
            parent(id)

    /** Extend the environment with new bindings. */
    def extend(mapping: Map[String, Int]) =
        new Env(this, mapping, null)

    override def toString =
        "Env(" + locals + ", " + globals + ", " + parent + ")"
}

object Env {
    val empty = new Env(null, null, null) {
        override def apply(id: String) =
            throw new Exception("Unknown identifier: " + id)

        override def toString = "EMPTY"
    }

    /**Creates new environment consisting of given set of local and global
      * variables. */
    def functionEnv(globals: List[String], locals: List[String]) = {
        val globalMap = globals.zip(Range(0, globals.size)).toMap
        val localMap = locals.zip(Range(0, locals.size).map(- _)).toMap
        new Env(empty, localMap, globalMap)
    }
}
