package io.svranesevic.scratchpad

sealed trait Lens[In, Out] { self =>

  def get(in: In): Out

  protected def update(in: In, out: Out): In

  final def modify(modify: Out => Out)(in: In): In = {
    val newOut = (self.get _ andThen modify.apply)(in)
    update(in, newOut)
  }

  final def andThen[Out1](that: Lens[Out, Out1]): Lens[In, Out1] =
    new Lens[In, Out1] {
      override def get(in: In): Out1 = (self.get _ andThen that.get _)(in)

      override protected def update(in: In, out1: Out1): In = {
        val out0    = self.get(in)
        val newOut0 = that.update(out0, out1)
        self.update(in, newOut0)
      }
    }
}

object Lens {

  def apply[In, Out](inToOut: In => Out, updateInWithOut: (In, Out) => In): Lens[In, Out] =
    new Lens[In, Out] {
      override def get(in: In): Out                       = inToOut(in)
      override protected def update(in: In, out: Out): In = updateInWithOut(in, out)
    }
}

object LensMain extends App {

  // Examples taken from https://blog.rockthejvm.com/lens/
  case class Guitar(make: String, model: String)
  case class Guitarist(name: String, favoriteGuitar: Guitar)
  case class RockBand(name: String, yearFormed: Int, leadGuitarist: Guitarist)

  // 1. Single Lens
  val kirksFavGuitar = Guitar("ESP", "M II")

  val guitarModelLens: Lens[Guitar, String] = Lens(_.model, (in, m) => in.copy(model = m))
  val kirksGuitarModel                      = guitarModelLens.get(kirksFavGuitar)
  assert(kirksGuitarModel == "M II")

  // 2. Composing Lenses
  val metallica = RockBand("Metallica", 1981, Guitarist("Kirk Hammett", Guitar("ESP", "M II")))

  val rockBandLeadLens: Lens[RockBand, Guitarist]  = Lens(_.leadGuitarist, (in, l) => in.copy(leadGuitarist = l))
  val guitaristGuitarLens: Lens[Guitarist, Guitar] = Lens(_.favoriteGuitar, (in, g) => in.copy(favoriteGuitar = g))
  val leadGuitarModel = rockBandLeadLens andThen guitaristGuitarLens andThen guitarModelLens

  val metallicasLeadFavGuitar = leadGuitarModel.get(metallica)
  assert(kirksGuitarModel == "M II")

  // 3. Modification
  val fixedGuitar = guitarModelLens.modify(_.replace(" ", "-"))(kirksFavGuitar)
  assert(fixedGuitar == Guitar("ESP", "M-II"))

  // 4. Composing Modifications
  val fixedLeadGuitarModel = leadGuitarModel.modify(_.replace(" ", "-"))(metallica)
  assert(fixedLeadGuitarModel == RockBand("Metallica", 1981, Guitarist("Kirk Hammett", Guitar("ESP", "M-II"))))
}
