package lancet.core

trait Base {
  type Rep[+T]
  type TypeRep[T]
  def repManifest[T:Manifest]: Manifest[Rep[T]]
}
