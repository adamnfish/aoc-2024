package com.adamnfish

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.adamnfish.days.Day01
import com.adamnfish.days.Day02
import sttp.client3.httpclient.cats.HttpClientCatsBackend
import sttp.client3.*
import scala.Console.*

import java.io.{File, FileWriter}
import java.nio.file.Files

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    val result =
      args match
        // solutions

        case "1" :: "1" :: input :: _ =>
          Day01.part1(input)

        case "1" :: "2" :: input :: _ =>
          Day01.part2(input)

        case "2" :: "1" :: input :: _ =>
          Day02.part1(input)

        // fetches the day's input and saves it to the correct location
        case "load" :: day :: Nil =>
          val paddedDayName = day.reverse.padTo(2, '0').reverse
          val outputFile = new File(
            s"puzzle-inputs/day$paddedDayName/input.txt"
          )
          val resources = for {
            backend <- HttpClientCatsBackend.resource[IO]()
            _ <- Resource.eval(
              IO.blocking(
                Files.createDirectories(outputFile.getParentFile.toPath)
              )
            )
            outputFileWriter <- Resource.make(
              IO(new FileWriter(outputFile))
            )(fw => IO(fw.close()))
          } yield (backend, outputFileWriter)
          resources.use: (backend, outputFileWriter) =>
            for {
              aocSessionOpt <- IO.envForIO.get("AOC_SESSION")
              aocSession <- IO.fromOption(
                aocSessionOpt
              )(
                new RuntimeException(
                  "Missing AOC_SESSION env var (get this from session cookie on AoC)"
                )
              )
              request = basicRequest
                .get(uri"https://adventofcode.com/2024/day/$day/input")
                .cookie("session", aocSession)
              response <- request.send(backend)
              body <- IO.fromEither(
                response.body.left.map(l =>
                  new RuntimeException(s"Couldn't parse input response: $l")
                )
              )
              _ <- IO.blocking(outputFileWriter.write(body))
            } yield s"Generated input for day $day"

        // unimplemented days
        case day :: puzzle :: _ =>
          IO.println(
            s"${RED}Day $day, puzzle ${BOLD}$puzzle${RESET}${RED} is not yet implemented${RESET}"
          ).as(())

        // prompt with help if no args supplied
        case _ =>
          for
            _ <- IO.println(s"Usage: run <day> <puzzle> <file>")
            _ <- IO.println(s"run 3 1 input")
            _ <- IO.println(s"(input is typically 'example' or 'input')")
            _ <- IO.println("or load day's input with")
            _ <- IO.println("Usage: run load <day>")
            _ <- IO.println("Usage: run load 5")
          yield "HELP TEXT"

    result.timed
      .flatTap { case (duration, result) =>
        import scala.Console._
        IO.println(
          s"${GREEN}Result: ${BOLD}$result${RESET}"
        ) >>
          IO.println(
            s"${YELLOW}Time: ${BOLD}${duration.toMillis}ms${RESET}"
          )
      }
      .as(ExitCode.Success)
}
