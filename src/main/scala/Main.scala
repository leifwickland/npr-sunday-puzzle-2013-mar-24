/**
 * Word Square Solver
 * @link http://en.wikipedia.org/wiki/Word_square
 */
case class Square(words: Seq[String]) extends AnyVal {
  def add(newWord: String): Option[Square] = {
    if (words.zipWithIndex.forall { case (word, i) => word(words.length) == newWord(i) }) Option(Square(words :+ newWord))
    else None
  }
  override def toString = ("Square(" +: words :+ ")").mkString("\n")
}

object Square {
  final val Empty = Square(Seq())
  def place(toPlace: List[List[String]], square: Square = Empty): Seq[Square] = {
    toPlace match {
      case Nil => Seq(square)
      case words :: more => words.flatMap(word => square.add(word).toSeq.flatMap(newSquare => place(more, newSquare)))
    }
  }
}

/**
 * Solves http://www.npr.org/2013/03/24/175144673/finding-the-answers-within
 */
object Main {
  def main(args: Array[String]) {
    val solutions = Square.place(List(
      middleN,
      middleA,
      middleS,
      middleA,
      middleL))
    solutions.foreach { s => println(s); println() }
  }

  def stringToList(s: String): List[String] = s.stripMargin.split(Array(' ', '\n')).toList.filter(_.nonEmpty)

  def middleS = List("nasal")

  // From http://www.visca.com/regexdict/
  def middleN = stringToList("""
    |banal
    |bandy
    |banns
    |canal
    |candy
    |canna
    |canny
    |dancy
    |dandy
    |fancy
    |fanny
    |ganja
    |handy
    |lanky
    |lynch
    |manat
    |mangy
    |manly
    |manna
    |manta
    |nancy
    |nanny
    |panda
    |pandy
    |pansy
    |panty
    |ranch
    |randy
    |rangy
    |sandy
    |synch
    |synth
    |tanka
    |tansy
    |""")

  def middleL = stringToList("""
    |allay
    |allyl
    |atlas
    |balas
    |balky
    |balmy
    |balsa
    |bylaw
    |calla
    |calyx
    |dally
    |galah
    |galax
    |halal
    |jalap
    |malar
    |pally
    |palmy
    |palsy
    |rally
    |ralph
    |salad
    |salal
    |sally
    |salsa
    |salty
    |splat
    |splay
    |sylph
    |sylva
    |talky
    |tally
    |walla
    |waltz
    |xylan
    |""")

  def middleA = stringToList("""
    |abaca
    |aback
    |abaft
    |abash
    |abaya
    |adapt
    |agama
    |alack
    |alarm
    |alary
    |amass
    |apart
    |asana
    |avast
    |award
    |awash
    |bhang
    |black
    |blaff
    |bland
    |blank
    |blast
    |bract
    |brand
    |brank
    |brant
    |brash
    |brass
    |brava
    |brawl
    |brawn
    |bwana
    |chaff
    |chalk
    |champ
    |chant
    |chaps
    |chard
    |charm
    |charr
    |chart
    |chary
    |chasm
    |clack
    |clamp
    |clang
    |clank
    |clary
    |clash
    |clasp
    |class
    |clast
    |crack
    |craft
    |cramp
    |crank
    |craps
    |crash
    |crass
    |crawl
    |crazy
    |draft
    |drama
    |drank
    |drawl
    |drawn
    |dwarf
    |flack
    |flaky
    |flamy
    |flank
    |flash
    |flask
    |flaxy
    |franc
    |frank
    |frass
    |gland
    |glans
    |glary
    |glass
    |gnarl
    |gnarr
    |gnash
    |graft
    |grama
    |gramp
    |grams
    |grana
    |grand
    |grant
    |graph
    |grasp
    |grass
    |gravy
    |knack
    |kraal
    |kraft
    |kvass
    |kyack
    |llama
    |nyala
    |plank
    |plant
    |plash
    |plasm
    |platy
    |playa
    |plaza
    |prang
    |prank
    |prawn
    |psalm
    |scald
    |scall
    |scalp
    |scaly
    |scamp
    |scant
    |scarf
    |scarp
    |scary
    |shack
    |shady
    |shaft
    |shaky
    |shank
    |shard
    |shark
    |sharp
    |shawl
    |shawm
    |skald
    |skank
    |slack
    |slang
    |slant
    |slash
    |slaty
    |smack
    |small
    |smalt
    |smarm
    |smart
    |smash
    |snack
    |snaky
    |snarf
    |snarl
    |spacy
    |spall
    |spang
    |spank
    |spark
    |spasm
    |spawn
    |stack
    |staff
    |stagy
    |stalk
    |stall
    |stamp
    |stand
    |stang
    |stank
    |staph
    |stark
    |start
    |stash
    |swamp
    |swank
    |sward
    |swarf
    |swarm
    |swart
    |swash
    |swath
    |thank
    |track
    |tract
    |tramp
    |trash
    |trass
    |trawl
    |twang
    |whack
    |whang
    |wharf
    |wrack
    |wrapt
    |wrath
    |""")
}

