
data PowerSource = Petrol | Pedal | Battery

data Vehicle : PowerSource -> Type where
     Unicycle: Vehicle Pedal
     Bicycle : Vehicle Pedal
     Car     : (fuel : Nat) -> Vehicle Petrol
     Bus     : (fuel : Nat) -> Vehicle Petrol
     Electric: (fuel : Nat) -> Vehicle Battery
     Motorcycle : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2
wheels (Electric fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

recharge : Vehicle Battery -> Vehicle Battery
recharge (Electric fuel) = Electric 100