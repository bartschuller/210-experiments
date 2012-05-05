import util.parsing.input.{Reader, PagedSeqReader}

case class ReaderWithState[S](delegate: PagedSeqReader,
                              extraState: S) extends Reader[Char] {
  override def source = delegate.source

  override def offset = delegate.offset

  def atEnd = delegate.atEnd

  def first = delegate.first

  def pos = delegate.pos

  def rest = copy(delegate = delegate.rest)
}
