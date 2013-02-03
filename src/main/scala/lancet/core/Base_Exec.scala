package lancet.core

trait Base_Exec extends Base {
  type Rep[+T] = T
  def repManifest[T:Manifest]: Manifest[T] = manifest[T]
}
