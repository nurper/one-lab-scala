package one.lab.tasks.week.two

import java.nio.file.Files
import java.nio.file.Paths

import scala.jdk.CollectionConverters._
import scala.util.chaining._
import scala.io.StdIn.readLine

/**
  * Можете реализовать свою логику.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  *                  то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  *                  cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  *                  по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive: Boolean = false
  }

  case class PrintErrorCommand(error: String) extends Command
  case class ListDirectoryCommand()           extends Command
  case class ListFilesCommand()               extends Command
  case class ListAllContentCommand()          extends Command

  case class ChangeDirectoryCommand(destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)

  def getAll(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .map(x => x.toFile.getName)
      .map(x => s"$path/$x")
      .toList
  }

  def getFiles(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(x => x.toFile.isFile)
      .map(x => x.toFile.getName)
      .map(x => s"$path/$x")
      .toList
  }
  def getDirectories(path: String): List[String] = {
    Files
      .list(Paths.get(path))
      .iterator()
      .asScala
      .filter(x => x.toFile.isDirectory)
      .map(x => x.toFile.getName)
      .map(x => s"$path/$x")
      .toList
  }
  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    val res = if(path.equals("..")) current.split('/').init.mkString("/") else s"$current/$path"
    if(Files.isDirectory(Paths.get(res))) Right(res)
    else Left(ChangePathError("No such directory"))
  }
  def parseCommand(input: String): Command = input match {
    case string: String if(string == "ll") => ListAllContentCommand()
    case string: String if(string == "ls") => ListFilesCommand()
    case string: String if(string == "dir") => ListDirectoryCommand()
    case string: String if(string.startsWith("cd ")) => ChangeDirectoryCommand(string.split(" ")(1))
    case _ => PrintErrorCommand("No such command")
  }
  def handleCommand(command: Command, currentPath: String): String = command match {
    case PrintErrorCommand(error) => error
    case ListAllContentCommand() => getAll(currentPath).mkString("\n")
    case ListDirectoryCommand() => getDirectories(currentPath).mkString("\n")
    case ListFilesCommand() => getFiles(currentPath).mkString("\n")
    case ChangeDirectoryCommand(destination) => changePath(currentPath, destination) match {
      case Left(value) => value.error
      case Right(value) => value
    }
  }
  def main(basePath: String): Unit = {
    def loop(path: String): Unit = {
      print(path+">")
      readLine().pipe(parseCommand).tap(x => {
        if(x.isSubstitutive) loop(handleCommand(x, path))
        else {
          println(handleCommand(x, path))
          loop(path)
        }
      })
    }

    loop(basePath)
  }

  main("C:/")

}
