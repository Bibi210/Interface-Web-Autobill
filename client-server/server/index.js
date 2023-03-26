const express = require("express");
const app = express();
const mongoose = require("mongoose");
const dotenv = require("dotenv");
const helmet = require("helmet");
const morgan = require("morgan");
const cors = require('cors');
const ocaml = require("../../MiniML_V2/_build/default/bin/main.bc.js");
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

app.listen(3002, () =>{
    console.log("running")
})

//npm start

const { exec } = require('child_process');
const MiniZinc = require('minizinc');



app.post('/api/run-code', async (req, res) => {
  const code = req.body.code;
  //const tempFile = './temp.ml';
  console.log("code get");
  try {
    let evaluation = {
      resultat: "",
      erreur: ""
    }
    evaluation = ocaml.ml.parse(code);
    console.log("code traite");
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5173');
    res.send(JSON.stringify({ result: evaluation }) );
  }catch(err){
    console.log(err.message);
    res.status(400).send(JSON.stringify({ error: err.message }));
  }

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
  
});

app.post('/api/minizinc', (req, res) => {
  const code = req.body.code;
  /*
  MiniZinc.init({
    // Executable name
    minizinc: 'minizinc',
    // Search paths (can omit to use PATH)
    minizincPaths: ['usr/bin']
  });

  const model = new MiniZinc.Model();
  model.addString(code);

  
  const solve = model.solve({
    options: {
      solver: 'gecode',
      timeout: 10000,
      statistics: true
    }
  });


  solve.on('solution', solution => console.log(solution.output.json));
  solve.on('statistics', stats => console.log(stats.statistics));
  solve.then(result => {
    console.log(result.solution.output.json);
    console.log(result.statistics);
  });

  console.log(code);

  res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5173');
  res.send(result.solution.output.json)
  */

  // Write the MiniZinc code to a temporary file
  const fs = require('fs');
  const tmpFile = './temp.mzn';
  fs.writeFileSync(tmpFile, code);

  // Run the MiniZinc code using the minizinc executable
  const cmd = `minizinc --solver Gecode ${tmpFile}`;
  exec(cmd, (error, stdout, stderr) => {
    fs.unlink('temp.mzn', (err) => {
      if (err) throw err;
      console.log('File deleted!');
    });
    
    // Check for errors afaire: test client full/client-server
    if (error || stderr) {
      const errorMsg = error ? error.message : stderr;
      console.error(`exec error: ${errorMsg}`);
      res.status(500).send(errorMsg);
      return;
    }

    
    console.log(stdout);
    // Send the MiniZinc output back to the client
    res.send(JSON.stringify({ result: stdout }) );
  });
});
