import * as React from "react"
import { RouteComponentProps } from "react-router-dom"
import "whatwg-fetch"
import { Chart } from "chart.js"
import ItemChart from "./ItemChart"
import * as moment from "moment"

interface DatasetProps {
  user : string
  name : string
  loadStatus : "loading" | "loaded" | "failed"
  saveStatus : "none" | "saving" | "saved" | "failed"
  dataset : DatasetValue | undefined
  items : Array<DataItem>
  saveDataItem : (dataItem : DataItem) => void
}

interface DatasetState {
  date : string
  fields : string[]
}

export interface DatasetValue {
  name : string
  fields : DatasetField[]
}

export interface DatasetField {
  name : string
}

export interface DataItem {
  datetime : string
  value : number
}

export class Dataset extends React.Component<DatasetProps, DatasetState> {
  constructor (props: DatasetProps) {
    console.log("Dataset constructor", props)
    super(props);
    let now = new Date();
    this.state = {
      date : "" + now.getFullYear() + "-" + this.padZero((now.getMonth() + 1)) + "-" + this.padZero(now.getDate()),
      fields : []
    };
  }

  render () : JSX.Element {
    console.log("Dataset.render", this)
    let { user, name, loadStatus, saveStatus, dataset, items } = this.props
    console.log(loadStatus, items)
    return <div>
             <form onSubmit={ e => this.saveDataItem(e) }>
               <label>
                 <span className="label">Date:</span> <input type="date" className="date" value={ this.state.date } onChange={ e => this.dateInputChange(e) } />
               </label><br />
               { dataset ? dataset.fields.map((field, ix) =>
                   <span key={ix}>
                     <label>
                       <span className="label">{ field.name }:</span> <input type="number" step="0.1" className="number" value={ this.state.fields[ix] } onChange={ e => this.numberInputChange(e, ix) } />
                     </label><br />
                   </span>
                 ) : ""
               }
               <input type="submit" value="Save" />
             </form>
             <ItemChart name={ name } items={ items} />
           </div>
           /*
             <h3>{ (saveStatus == "saving" ? "Saving..." :
                    saveStatus == "saved"  ? "Saved" :
                    saveStatus == "failed"  ? "Error saving" : ""
                   ) }
             </h3>
             <h2>Hello {user}!</h2>
             <h3>{ (loadStatus == "loading" ? "Loading" :
                    loadStatus == "loaded"  ? "Viewing" :
                    loadStatus == "failed"  ? "Error loading" : "Unknown status for"
                   ) + " dataset " + name}
             </h3>
             <ul>
             { data.map((item, ix) => <DataListItem key={ix} data={item}/>) }
             </ul>
            */
  }

  dateInputChange (e : React.ChangeEvent<HTMLInputElement>) {
    let val : string = e.target.value;
    this.setState( {
      "date": val
    })
  }

  numberInputChange (e : React.ChangeEvent<HTMLInputElement>, ix : number) {
    let val : string = e.target.value;
    const { fields } = this.state;
    fields[ix] = val;
    this.setState({ fields });
  }

  padZero (num: number) : string {
    if (num < 10)
      return "0" + num;
    else
      return "" + num;
  }

/*
    let strVal : string = e.target.value;
    let mVal : number = Date.parse(strVal);
    if (!isNaN(mVal)) {
      this.setState( {
        "date": new Date(mVal)
      })
    }
  }

  numberInputChange (e : React.ChangeEvent<HTMLInputElement>) {
    let strVal : string = e.target.value;
    let mVal : number = parseInt(strVal);
    if (!isNaN(mVal)) {
      this.setState( {
        "number": mVal
      })
    }
  }
*/

  saveDataItem (e : React.FormEvent<HTMLFormElement>) : void {
    console.log(e);
    e.preventDefault();
    let dateInput : HTMLInputElement = e.currentTarget.getElementsByClassName("date")[0] as HTMLInputElement;
    let numberInput : HTMLInputElement = e.currentTarget.getElementsByClassName("number")[0] as HTMLInputElement;
    let date : number = Date.parse(dateInput.value);
    let num : number = parseFloat(numberInput.value);
    let hasError = false;
    if (isNaN(date)) {
      hasError = true;
      // TODO show error
    }
    if (isNaN(num)) {
      hasError = true;
      // TODO show error
    }
    if (!hasError) {
      this.props.saveDataItem({ datetime: new Date(date).toISOString(), value: num });
    }
  }
}

const DataListItem = (props : { data: DataItem }) => (
  <li>{props.data.datetime + ": " + props.data.value}</li>
)

export default Dataset
