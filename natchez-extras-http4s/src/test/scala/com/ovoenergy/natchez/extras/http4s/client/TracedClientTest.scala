package com.ovoenergy.natchez.extras.http4s.client

import cats.data.Kleisli
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.ovoenergy.natchez.extras.http4s.Configuration
import com.ovoenergy.natchez.extras.testkit.TestEntryPoint
import com.ovoenergy.natchez.extras.testkit.TestEntryPoint.TestSpan
import natchez.{Kernel, Span}
import org.http4s.Request
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TracedClientTest extends AnyWordSpec with Matchers {

  val unit: Kleisli[IO, Span[IO], Unit] = Kleisli.pure(())
  val config: Configuration[IO] = Configuration.default[IO]()
  type TraceIO[A] = Kleisli[IO, Span[IO], A]

  "TracedClient" should {

    "Add the kernel to requests" in {

      val requests: List[Request[IO]] = (
        for {
          client <- TestClient[IO]
          ep    <- TestEntryPoint[IO]
          http   = TracedClient(client.client, config)
          kernel = Kernel(Map("X-Trace-Token" -> "token"))
          _      <- ep.continue("bar", kernel).use(http.named("foo").status(Request[TraceIO]()).run)
          reqs   <- client.requests
        } yield reqs
      ).unsafeRunSync()

      requests.forall(_.headers.headers.exists(_.name.equals("X-Trace-Token"))) shouldBe true
    }

    "Create a new span for HTTP requests" in {

      val spans: List[TestSpan] = (
        for {
          client <- TestClient[IO]
          ep    <- TestEntryPoint[IO]
          http   = TracedClient(client.client, config)
          _      <- ep.root("root").use(http.named("foo").status(Request[TraceIO]()).run)
          reqs   <- ep.spans
        } yield reqs
      ).unsafeRunSync()

      spans.length shouldBe 2
      spans.head.name shouldBe "foo:http.request:/"
    }
  }
}
