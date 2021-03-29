import * as React from "react"
import { RouteComponentProps } from "react-router-dom"

import { Dataset, DataItem, DatasetValue } from "./Dataset"

interface DatasetContainerState {
  loadStatus : "loading" | "loaded" | "failed"
  saveStatus : "none" | "saving" | "saved" | "failed"
  dataset : DatasetValue | undefined
  items : Array<DataItem>
}

class DatasetContainer extends React.Component<RouteComponentProps<any>> {
  state : DatasetContainerState = {
    loadStatus : "loading",
    saveStatus : "none",
    dataset : undefined,
    items : []
  }

  componentDidMount () {
    this.reloadData();
  }

  getItemUrl () : string {
    return this.getDatasetUrl() + "/item"
  }

  getDatasetUrl () : string {
    return "/api/user/" + this.props.match.params.user
         + "/dataset/" + this.props.match.params.dataset
  }

  reloadData () {
    Promise.all([fetch(this.getDatasetUrl()), fetch(this.getItemUrl())])
      .then(responses => Promise.all([responses[0].json(), responses[1].json()]))
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
                    items={this.state.items}
                    dataset={this.state.dataset}
                    saveDataItem={this.saveDataItem.bind(this)}
           />
  }

  renderResponse ([dsJson, isJson] : any[]) : void {
    console.log("Loaded data: ", dsJson, isJson)
    this.setState((prevState) => ({
      loadStatus: "loaded",
      items: isJson,
      dataset: dsJson
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
