package me.nsmr
package console

object SampleConsole extends Console {
  val version = Command("version") { _ => println("version 1.0.0") }
  val help = Command("help") { _ => println("This program is Sample of Simple Console Library.") }

  override val initialMessage: Option[String] = Some("Simple Console Library Sample")
  override val commands = List(version, help)

}
