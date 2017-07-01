import * as React from "react"
import { RouteComponentProps } from "react-router-dom"

import { Dataset, DataItem } from "./Dataset"

interface DatasetContainerState {
    status : "loading" | "loaded" | "failed"
    data : Array<DataItem>
}

class DatasetContainer extends React.Component<RouteComponentProps<any>> {
  state : DatasetContainerState = {
    status : "loading",
    data : []
  }

  componentDidMount () {
    fetch( "/api/user/" + this.props.match.params.user
         + "/dataset/" + this.props.match.params.dataset + "/item"
         ).then(response => response.json())
         .then(this.renderResponse)
         .catch(this.renderError)
  }

  render () : JSX.Element {
    return <Dataset user={this.props.match.params.user}
                    name={this.props.match.params.dataset}
                    status={this.state.status}
                    data={this.state.data}
           />
  }

  renderResponse = (json : any) => {
    console.log(json)
    this.setState((prevState) => ({
      status: "loaded",
      data: json
    }))
  }

  renderError (error : Error) {
    console.log(error)
  }
}

export default DatasetContainer
