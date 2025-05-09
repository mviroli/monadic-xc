package scafi

object Devices:
  opaque type Device = Int
  val selfDevice: Device = 0
  type Domain = Set[Device]
  private var counter: Device = 1
  def newDevice(): Device = try
    counter
  finally
    counter = counter + 1
