import * as React from "react"
import { RouteComponentProps } from "react-router-dom"

import { Dataset, DataItem } from "./Dataset"

interface DatasetContainerState {
  loadStatus : "loading" | "loaded" | "failed"
  saveStatus : "none" | "saving" | "saved" | "failed"
  data : Array<DataItem>
}

class DatasetContainer extends React.Component<RouteComponentProps<any>> {
  state : DatasetContainerState = {
    loadStatus : "loading",
    saveStatus : "none",
    data : []
  }

  componentDidMount () {
    this.reloadData();
  }

  getItemUrl () : string {
    return "/api/user/" + this.props.match.params.user
         + "/dataset/" + this.props.match.params.dataset + "/item"
  }

  reloadData () {
    fetch(this.getItemUrl())
      .then(response => response.json())
      .then(this.renderResponse.bind(this))
      .catch(this.renderError.bind(this))
  }

  saveDataItem (dataItem : DataItem) {
    this.setState({
      saveState: "saving"
    })
    let headers = new Headers();
    headers.set("Content-Type", "application/json");
    fetch( this.getItemUrl()
         , { method : "POST"
           , headers: headers
           , body: JSON.stringify(dataItem)
           }
         )
      .then(response => response.json())
      .then(this.dataItemSaved.bind(this))
      .catch(this.dataItemSaveFailed.bind(this))
      .then(this.reloadData.bind(this));
  }

  dataItemSaved (json : any) : void {
    console.log("Item saved", json);
    this.setState({
      saveStatus: "saved"
    });
  }

  dataItemSaveFailed (error : Error) : void {
    console.log("Error saving item", error);
    this.setState({
      saveStatus: "failed"
    });
  }

  render () : JSX.Element {
    return <Dataset user={this.props.match.params.user}
                    name={this.props.match.params.dataset}
                    loadStatus={this.state.loadStatus}
                    saveStatus={this.state.saveStatus}
                    data={this.state.data}
                    saveDataItem={this.saveDataItem.bind(this)}
           />
  }

  renderResponse (json : any) : void {
    console.log("Loaded data: ", json)
    this.setState((prevState) => ({
      loadStatus: "loaded",
      data: json
    }))
  }

  renderError (error : Error) : void {
    console.log("Error loading data: ", error)
    this.setState({
      loadStatus: "failed"
    });
  }
}

export default DatasetContainer
