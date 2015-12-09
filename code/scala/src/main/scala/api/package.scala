
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import spray.http.MediaTypes._
import spray.http.{HttpEntity, ContentType}
import spray.httpx.marshalling.{Marshaller, MarshallingContext}
import spray.httpx.unmarshalling._

import scala.reflect.ClassTag


package object api {
  val objectMapper = new ObjectMapper()
    .registerModule(DefaultScalaModule)

  def makeMarshaller[T] = new Marshaller[T] {
    def apply(value: T, ctx: MarshallingContext) {
      val bytes = objectMapper.writeValueAsBytes(value)
      ctx.marshalTo(
        entity = HttpEntity(ContentType.apply(`application/json`), bytes)
      )
    }
  }

  def makeUnmarshaller[T](implicit ct: ClassTag[T]) = new Unmarshaller[T] {
    def apply(entity: HttpEntity) = {
      val buffer = entity.data.toByteArray

      try {
        val result = objectMapper.readValue[T](buffer, ct.runtimeClass.asInstanceOf[Class[T]])
        Right(result)
      } catch {
        case e: Throwable =>
          e.printStackTrace
          Left(MalformedContent("didnt make it", e))
      }
    }
  }

  implicit val textSubmitMarshaller = makeUnmarshaller[TextSubmit]
}
