package stapl.performance

import org.joda.time.LocalDateTime
import stapl.core.AbstractPolicy
import stapl.core.Deny
import stapl.core.NotApplicable
import stapl.core.boolean2Value
import stapl.core.dateTime2Value
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.PDP
import stapl.core.string2Value
import stapl.core.stringSeq2Value
import stapl.parser.PolicyParser
import scala.util.Success

object ExtLoadingTest extends App {

  val policySizes = List(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200)

  val policyHome = args(0)
  val policyName = args(1)
  val nbWarmups = args(2).toInt
  val nbRuns = args(3).toInt

  if (policyName == "ehealth") {
    test("ehealth", policyHome + "/ehealth.stapl", testEhealthPolicy, parseEhealth, nbWarmups, nbRuns)
  } else if (policyName == "edocs") {
    test("edocs", policyHome + "/edocs.stapl", testEdocsPolicy, parseEdocs, nbWarmups, nbRuns)
  } else {
    println("Invalid arguments")
  }

  def testEhealthPolicy(policy: AbstractPolicy) = {
    import EhealthAttributes._

    val attributeFinder = new AttributeFinder

    val pdp = new PDP(policy, attributeFinder)
    val result = pdp.evaluate("maarten", "view", "doc123",
      subject.roles -> List("medical_personnel", "nurse"),
      subject.triggered_breaking_glass -> false,
      subject.department -> "elder_care",
      subject.allowed_to_access_pms -> true,
      subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
      subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
      subject.location -> "hospital",
      subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
      subject.responsible_patients -> List("patientY", "patientZ"),
      resource.owner_id -> "patientX",
      resource.owner_withdrawn_consents -> List("subject1"),
      resource.type_ -> "patientstatus",
      resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
      env.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    if (result.decision != Deny) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def testEdocsPolicy(policy: AbstractPolicy) = {
    import EdocsAttributes._

    val attributeFinder = new AttributeFinder

    val pdp = new PDP(policy, attributeFinder)
    val result = pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("junior"),
        subject.department -> "another-department",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank")
    if (result.decision != Deny) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def testArtificialPolicy(policy: AbstractPolicy) = {
    import ArtificialAttributes._

    val attributeFinder = new AttributeFinder
    attributeFinder += new ArtificialAttributeFinderModule

    val pdp = new PDP(policy, attributeFinder)
    val result = pdp.evaluate("maarten", "create", "subtenantX")
    if (result.decision != NotApplicable) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def read(path: String) = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    policyString
  }

  def test(label: String, path: String, testPolicy: (AbstractPolicy) => Unit, parse: String => AbstractPolicy, nbWarmups: Int, nbRuns: Int) = {
    val timer = new Timer
    import timer.time

    val policyString = read(path)

    // TODO programatically disable log output on stdout
    
    // test the policy
    val policy = time { parse(policyString) }
    testPolicy(policy)
    
    // do the warmups
    for(i <- 0 until nbWarmups) {
      parse(policyString)
    }

    println(s"Initial loading time = ${timer.mean} ms")

    timer.reset
    for (i <- 0 until nbRuns) {
      time { parse(policyString) }
    }

    println(f"Loading - STAPL - $label - ${timer.mean}%2.2f ms ($nbWarmups warmups, $nbRuns runs)")
    //println(f"   (details: stdDev = ${timer.stdDev}%2.2f, confInterval = ${timer.confInt() * 100}%2.2f%%)")
    //println(s"Timings: ${timer.timings.reverse}")

  }
  
  def parseEdocs(policyString: String): AbstractPolicy = {
    import EdocsAttributes._
    
    val parser = new PolicyParser(policyString, subject, action, resource, env)
    val Success(result) = parser.Stapl.run()
    result
  }
  
  def parseEhealth(policyString: String): AbstractPolicy = {
    import EhealthAttributes._
    
    val parser = new PolicyParser(policyString, subject, action, resource, env)
    val Success(result) = parser.Stapl.run()
    result
  }
}