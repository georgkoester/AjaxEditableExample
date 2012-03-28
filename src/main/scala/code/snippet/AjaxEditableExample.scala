package code
package snippet

import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import net.liftweb.http.js.JsCmds.Alert._
import net.liftweb.http.{StatefulSnippet, RequestVar, SHtml, S}
import net.liftweb.http.js.JsCmds.{Replace, SetHtml, Alert, Noop}
import net.liftweb.http.js.JsCmd
import net.liftweb.common.{Empty, Box, Loggable, Full}


object MySHtml extends SHtml {

  def slowNetworkSimulationAjaxEditable (displayContents : => NodeSeq, editForm : => NodeSeq, onSubmit : () => JsCmd) : NodeSeq = {
    import net.liftweb.http.js
    import js.{jquery,JsCmd,JsCmds,JE}
    import jquery.JqJsCmds
    import JsCmds.{Noop,SetHtml}
    import JE.Str
    import JqJsCmds.{Hide,Show}

    val divName = Helpers.nextFuncName
    val dispName = divName + "_display"
    val editName = divName + "_edit"

    def swapJsCmd (show : String, hide : String) : JsCmd = Show(show) & Hide(hide)

    def setAndSwap (show : String, showContents : => NodeSeq, hide : String) : JsCmd =
      (SHtml.ajaxCall(Str("ignore"), {ignore : String => try {Thread.sleep(2000);} catch {case _ => }
        SetHtml(show, showContents)})._2.cmd & swapJsCmd(show,hide))

    def displayMarkup : NodeSeq =
      displayContents ++ Text(" ") ++
          <input value={S.??("edit")} type="button" onclick={setAndSwap(editName, editMarkup, dispName).toJsCmd + " return false;"} />

    def editMarkup : NodeSeq = {
      val formData : NodeSeq =
        editForm ++
            <input type="submit" value={S.??("ok")} /> ++
          hidden(onSubmit) ++
            <input type="button" onclick={swapJsCmd(dispName,editName).toJsCmd + " return false;"} value={S.??("cancel")} />

      ajaxForm(formData,
        Noop,
        setAndSwap(dispName, displayMarkup, editName))
    }

    <div>
      <div id={dispName}>
        {displayMarkup}
      </div>
      <div id={editName} style="display: none;">
        {editMarkup}
      </div>
    </div>
  }

  def myAjaxEditable (displayContents : => NodeSeq, editForm : => NodeSeq, onSubmit : () => JsCmd,
                      loadingContents: => NodeSeq = Text("...loading..."),
                      onDelete : Box[() => JsCmd] = Empty) : NodeSeq = {
    import net.liftweb.http.js
    import js.{jquery,JsCmd,JsCmds,JE}
    import jquery.JqJsCmds
    import JsCmds.{Noop,SetHtml}
    import JE.Str
    import JqJsCmds.{Hide,Show}

    val divName = Helpers.nextFuncName
    val dispName = divName + "_display"
    val editName = divName + "_edit"

    def swapJsCmd (show : String, hide : String) : JsCmd = Show(show) & Hide(hide)

    def setAndSwap (show : String, showContents : => NodeSeq, hide : String,
                    showLoadingContents: => Box[NodeSeq] = Empty) : JsCmd = {
      (replacementCmd (show, showContents) &
        (showLoadingContents.map(SetHtml (show, _) ) openOr Noop ) )  &
        swapJsCmd(show,hide)
    }

    def replacementCmd(where:String, what: => NodeSeq) = {
      SHtml.ajaxCall(Str("ignore"), {ignore : String => try {Thread.sleep(2000);} catch {case _ => }
        SetHtml(where, what)
      })._2.cmd
    }

    def displayMarkup : NodeSeq =
      displayContents ++ Text(" ") ++
          <input value={S.??("edit")} type="button" onclick={
      setAndSwap(editName, editMarkup, dispName, Full(loadingContents)).toJsCmd + " return false;"} />

    def editMarkup : NodeSeq = {
      val formData : NodeSeq =
        editForm ++
            <input type="submit" value={S.??("ok")} /> ++
          hidden(onSubmit) ++
            <input type="button" onclick={
        swapJsCmd(dispName,editName).toJsCmd + " return false;"} value={S.??("cancel")} /> ++
          (onDelete.map (delFunc => <input type="button" onclick={
          (Hide(editName) &
            SHtml.ajaxCall(Str("ignore"), {ignore : String => delFunc()})._2.cmd).toJsCmd + " return false;"
          } value={S.??("delete")} />
          ) openOr NodeSeq.Empty)

      ajaxForm(formData,
        Noop,
        setAndSwap(dispName, displayMarkup, editName, Full(loadingContents)))
    }

    <div>
      <div id={dispName}>
        {displayMarkup}
      </div>
      <div id={editName} style="display: none;">
        {Text("This site requries javascript")}
      </div>
    </div>
  }
  
}

class AjaxEditableExample extends StatefulSnippet with Loggable {

  var text1 = "t1"
  var text2 = "t2"
  var text3 = "t3"

  def dispatch = {
    case "render" => render
  }

  def render =
    "* *" #> (Text("Original:") ++ SHtml.ajaxEditable(Text(text1),
      SHtml.text(text1, (v) => text1 = v), () => {
        Noop
      }) ++
      Text("Original with 2sec server delay:") ++ MySHtml.slowNetworkSimulationAjaxEditable(Text(text2),
        SHtml.text(text2, (v) => text2 = v), () => {
          Noop
        }) ++
      Text("Original with changes and simulated 2 sec sever delay:") ++ MySHtml.myAjaxEditable(Text(text3),
        SHtml.text(text3, (v) => text3 = v), () => {
          Noop
        })
      )

}

