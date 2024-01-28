import cats.*
import cats.data.Reader
import com.github.tarao.record4s.%

val DBUrl = "jdbc://windy:melt@localhost:3306/db"
val OpenAIAPI = "deadbeef66666666"
val env = %(DBUrl, OpenAIAPI)

env

val chat = Reader[% { val OpenAIAPI: String }, String] { e =>
  println(s"connecting to ChatGPT using ${e.OpenAIAPI}...")
  "Hi! I'm ChatGPT!!!"
}

chat(env)

val list = Reader[% { val DBUrl: String }, List[String]] { e =>
  println(s"connecting to DB ${e.DBUrl}...")
  List("foo", "bar", "buzz")
}

list(env)

// usingを使うこともできるが

def chatImp(using e: % { val OpenAIAPI: String }): String = {
  println(s"connecting to ChatGPT using ${OpenAIAPI}...")
  "Hi! I'm ChatGPT!!!"
}

def listImp(using e: % { val DBUrl: String }): List[String] = {
  println(s"connecting to DB ${DBUrl}...")
  List("foo", "bar", "buzz")
}

// 渡す側でこういう構成やりたくない
def multipleTask(using
                 e: % { val OpenAIAPI: String; val DBUrl: String }
                ): Unit = {
  chatImp
  listImp
}

{
  type Env = % { val OpenAIAPI: String; val DBUrl: String }
  given envGiven: Env = env
  multipleTask
}
// 渡す環境引数の型を省略できないか
object ChatModule:
  type R = % { val OpenAIAPI: String }
  def chatImp2[E <: R](using e: E): String = {
    println(s"connecting to ChatGPT using ${OpenAIAPI}...")
    "Hi! I'm ChatGPT!!!"
  }

object ListModule:
  type R = % { val DBUrl: String }
  def listImp2[E <: R](using e: E): List[String] = {
    println(s"connecting to DB ${DBUrl}...")
    List("foo", "bar", "buzz")
  }

val r = %(foo = "bar", bar = 42)

// generalized type constraints
def getR[R](rec: R)(using R <:< %{val foo: String}) =
  rec.foo

// 通常にrを渡せる。
getR(r)

// 型を書くのが手間なのでobjectにtype aliasとともに押し込める。
object FooOperation:
  type E = %{val foo: String}
  def getFoo[R](rec: R)(using R <:< E) =
    rec.foo

// これもまた普通に呼び出せる。
FooOperation.getFoo(r)

// objectに環境型を押し込めたので引数からは見えない
def useFooOps(rec: FooOperation.E) =
  FooOperation.getFoo(rec)

// 呼べる
useFooOps(r)

// 同様に別の操作もobjectに閉じ込める
object BarOperation:
  type E = %{val bar: Int}
  def incBar[R](rec: R)(using R <:< E) = rec.bar + 1

def useBarOps(rec: BarOperation.E) =
  BarOperation.incBar(rec)

useBarOps(r)

// さて、依存性が複数のオブジェクトに分散したので、両方を呼び出したいときは&でintersection typeを取ればよい。
def useFooBarOps(rec: FooOperation.E & BarOperation.E) =
  println(FooOperation.getFoo(rec))
  println(BarOperation.incBar(rec))

useFooBarOps(r)

// この形はコンストラクタ・インジェクションなので、using句で自動的に注入できる。
def useFooBarOpsAuto(using rec: FooOperation.E & BarOperation.E) =
  println(FooOperation.getFoo(rec))
  println(BarOperation.incBar(rec))

{
  type Rec = %{ val foo: String; val bar: Int }
  given recGiven: Rec = r // いちどtype aliasを挟まないといけない
  useFooBarOpsAuto
}

// 具体的な例で実験してみよう
object OpenAIOperation:
  type Dep = %{ val openAI: %{ val apiKey: String }}

  def complete[R <: Dep](prompt: String)(using dep: R): String =
    s"(using ${dep.openAI.apiKey}) but I hate you"

object Twitter:
  type Dep = %{ val twitter: %{ val apiKey: String } }

  def tweet[R <: Dep](text: String)(using dep: R): Unit = println(s"Tweet from AI: ${text}")

// Use both operation; same as other operation
object TweetCompletionOperation:
  type Dep = OpenAIOperation.Dep & Twitter.Dep
  def completeAndTweet[R <: Dep](prompt: String)(using dep: R): Unit =
    val completeResult = OpenAIOperation.complete[R](prompt)
    val tweet = Twitter.tweet[R](completeResult)

// Usage
{
  type Dep = %{
    val openAI: %{ val apiKey: String }
    val twitter: %{ val apiKey: String }
  }
  given Dep = %(openAI = %(apiKey = "12345"), twitter = %(apiKey = "42"))
  TweetCompletionOperation.completeAndTweet("foo")
}