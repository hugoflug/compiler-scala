object EitherUtils {
  def orErr[T, E](opt: Option[T],  err: E): Either[E, T] =
    opt match {
      case Some(value) => Right(value)
      case None => Left(err)
    }

  def assert[E](b: Boolean,  err: E): Either[E, Unit] = if (b) Right() else Left(err)

  def orFirstError[A, B](eithers: Seq[Either[A, B]]): Either[A, Seq[B]] = {
    val lefts = eithers.collect({ case left: Left[A, B] => left })
    if (lefts.nonEmpty) Left(lefts.head.left.get)
    else Right(eithers.map(_.right.get))
  }

}
