import Cocoa
import Foundation

/*: Метод ломанных

 */

func f(x: Double) -> Double {
    return x*x - 2*4*x + 16 + log(x)
}

func df(x: Double) -> Double {
    return 2*x - 8 + 1/x
}

func lipschitz() -> Double {
    var max: Double
    max = fabs(df(x: 3))
    
    for i in 3..<6 {
        if max < fabs(df(x: Double(i))) {
            max = fabs(df(x: Double(i)))
        }
    }
    return max
}

func minIndex(v: Array<Double>) -> Int {
    var min: Int = 1
    for i  in 1..<v.count {
        if (v[i] < v[min]) {
            min = i
        }
    }
    return min
}

var x: Double
var a: Double
var b: Double
var R: Double
var m: Double

var ε: Double
var Δ: Double
var x𝞮: Double

var n = 1

var Mn = [Double]()
var Xn = [Double]()

a = 3
b = 5
ε = 1e-5

R = lipschitz()

x = (a + b) / 2 + (f(x: a) - f(x: b)) / (2*R)
m = (a - b) * R / 2 + (f(x: a) - f(x: b)) / 2


while ((f(x: x) - m) > ε) {
    
    Δ = (f(x: x) - m) / (2*R)
    
    Xn.append(x - Δ)
    Xn.append(x + Δ)
    Mn.append((f(x: x) + m) / 2)
    Mn.append((f(x: x) + m) / 2)
    
    let k = minIndex(v: Mn)
    
    x = Xn[k]
    m = Mn[k]
    
    n += 1
    
    Mn.remove(at: k)
    Xn.remove(at: k)
    
    print("iter", n)
    print("xmin = ", x)
    print("m = ", m)
    print("======================")
    
}

x𝞮 = x
print("xmin = ", x𝞮)
print("fmin = ", f(x: x𝞮))
print("m = ", m)
print("R = ", R)


