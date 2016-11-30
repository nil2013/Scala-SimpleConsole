package me.nsmr
package console

trait Console {
  def commands: List[Command]
  def initialMessage: Option[String] = None
  def prompt: String = "> "

  def main(args: Array[String]): Unit = {
    if(args.size > 0) {
      args.foreach { run }
    } else {
      // コンソールウィンドウを出す
      initialMessage.foreach(println)
      print(prompt)
      scala.io.Source.stdin.getLines().takeWhile(line => line != "exit").foreach { line =>
        run(line)
        print(prompt)
      }
    }
  }

  def run(command: String): Unit = {
    def error(mes: String): Unit = Colors.Red.using(System.err)(_.println(mes))
    val pat = """(\S+)\s*(.*)""".r
    command match {
      case pat(cmd, args) =>
        commands.find(_.name == cmd) match {
          case Some(cmd) => cmd(args.split(" "))
          case None => error(s"[error] command not found: '${cmd}'")
        }
      case cmd if !cmd.powerTrim.isEmpty => error(s"[error] command not found: '${cmd}'")
      case _ =>
    }
  }

  trait Command {
    def name: String
    def apply(args: Array[String]): Unit
  }
  class SimpleCommand(override val name: String, body: Array[String] => Unit) extends Command {
    def apply(args: Array[String]): Unit = body(args)
  }
  object Command {
    def apply(name: String)(body: Array[String] => Unit): Command = new SimpleCommand(name, body)
  }
}
