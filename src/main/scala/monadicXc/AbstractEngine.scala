package monadicXc

trait AbstractEngine:
  // engine abstract concepts
  type Device
  case class NValue[+A](a: A, map: Map[Device, A] = Map()):
    def apply(device: Device): A =
      if map.contains(device) then map(device) else a

  type Message
  type Context = Map[Device, Message]

  trait Aggregate[A]:
    def run: Device => Context => (Context, A, Message)