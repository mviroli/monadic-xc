package monadicXc

trait Devices:

  opaque type Device = Int
  val selfDevice: Device = 0
  private var currentDevice: Device = 1

  def newCounter: Device =
    try currentDevice finally currentDevice = currentDevice + 1
