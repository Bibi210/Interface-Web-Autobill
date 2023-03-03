const express = require("express");
const app = express();
const mongoose = require("mongoose");
const dotenv = require("dotenv");
const helmet = require("helmet");
const morgan = require("morgan");
const cors = require('cors');
const ocaml = require('../front/ocaml/main.js');
//const { lcbpv } = require("../language/mllike")
//const billPrompts = require("../data/billPrompt")

dotenv.config();

mongoose.connect(process.env.MONGO_URL, 
    {useNewUrlParser: true, useUnifiesTopology: true},
      ()=>{
    console.log("Connected to MongoDB")
});

//middleware
app.use(express.json());
app.use(helmet());
app.use(morgan("common"));
app.use(cors({ origin: 'http://localhost:5173' }));


mongoose.set('strictQuery', true);

app.listen(3001, () =>{
    console.log("running")
})

//npm start

const { spawn } = require('child_process');
const fs = require('fs');



app.post('/api/run-code', async (req, res) => {
  const code = req.body.code;
  console.log(code);
  //const tempFile = './temp.ml';
  console.log("code get");
  let evaluation = ocaml.ml.parse(code);
  /*const evalCode = (code) => {
    let topLevelResult = {
      types : "",
      resultat: "",
      erreurs: ""
    }
    import("../front/ocaml/main.js").then((ml) => {
      // Call the exported function
      
      console.log(evaluation);
    }).catch((error) => {
      console.error(error);
    });
    //evaluation = ml.parse(code)
    console.log(evaluation)
    //setPrint(evaluation.resultat)
    // setTypes(evaluation.types)
    // setPrint(evaluation.resultat == "" ? evaluation.erreurs : evaluation.resultat)
    
  }*/

  /*
  await fs.writeFile(tempFile, code, (err) => {
    if (err) throw err;
    console.log('The file has been saved!');
  });

  
  const jsCode = await new Promise((resolve, reject) => {
    const ocaml = spawn('ocamlfind', ['ocamlc', '-package', 'js_of_ocaml', '-o', 'temp.js', 'temp.ml']);
    ocaml.on('close', (code) => {
      if (code === 0) {
        fs.readFile('./temp.js', 'utf8', (err, data) => {
          if (err) {
            reject(err);
          } else {
            resolve(data);
          }
        });
      } else {
        reject(`ocamlc exited with code ${code}`);
      }
    });
  })
    .catch((err) => {
      console.error(err);
    });
  
  */
  console.log("code traite");
  res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5173');
  console.log(evaluation);
  res.send(JSON.stringify({ result: evaluation }) );
});
