import * as React from "react"
import { RouteComponentProps } from "react-router-dom"
import "whatwg-fetch"

interface DatasetProps {
  user : string
  name : string
  status : "loading" | "loaded" | "failed"
  data : Array<DataItem>
}

export interface DataItem {
  datetime : string
  value : number
}

export class Dataset extends React.Component<DatasetProps> {
  render () : JSX.Element {
    let { user, name, status, data } = this.props
    console.log(status, data)
    return <div>
             <h2>Hello {user}!</h2>
             <h3>{ (status == "loading" ? "Loading" :
                    status == "loaded"  ? "Viewing" :
                    status == "failed"  ? "Error loading" : "Unknown status for"
                   ) + " dataset " + name}</h3>
             <ul>
             { data.map(item => <DataListItem key={item.datetime} data={item}/>) }
             </ul>
           </div>
  }
}

const DataListItem = (props : { data: DataItem }) => (
  <li>{props.data.datetime + ": " + props.data.value}</li>
)

export default Dataset
