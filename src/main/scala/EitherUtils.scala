object EitherUtils {
  def orFirstError[A, B](eithers: Seq[Either[A, B]]): Either[A, Seq[B]] = {
    val lefts = eithers.collect({ case left: Left[A, B] => left })
    if (lefts.nonEmpty) Left(lefts.head.left.get)
    else Right(eithers.map(_.right.get))
  }
}
