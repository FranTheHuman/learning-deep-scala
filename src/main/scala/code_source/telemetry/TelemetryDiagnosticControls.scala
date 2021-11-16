package code_source.telemetry

import scala.beans.{BeanProperty, BooleanBeanProperty}

class TelemetryDiagnosticControls {

  private val DiagnosticChannelConnectionString: String = "*111#"

  private val telemetryClient: TelemetryClient = new TelemetryClient()

  @BeanProperty
  var diagnosticInfo: String = ""

  def checkTransmission(): Unit = {
    diagnosticInfo = ""
    telemetryClient.disconnect()
    var retryLeft: Int = 3
    while (telemetryClient.getOnlineStatus == false && retryLeft > 0) {
      telemetryClient.connect(DiagnosticChannelConnectionString)
      retryLeft -= 1
    }
    if (telemetryClient.getOnlineStatus == false) {
      throw new Exception("Unable to connect.")
    }
    telemetryClient.send("AT#UD")
    diagnosticInfo = telemetryClient.receive()
  }

}
