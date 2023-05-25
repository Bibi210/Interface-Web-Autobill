const express = require("express");
const app = express();
const mongoose = require("mongoose");
const dotenv = require("dotenv");
const helmet = require("helmet");
const morgan = require("morgan");
const cors = require('cors');
const ocaml = require("../MiniML/_build/default/bin/main.bc.js"); 

dotenv.config();


//middleware
app.use(express.json());
app.use(helmet());
app.use(morgan("common"));
app.use(cors({ origin: 'http://localhost:5174' }));


mongoose.set('strictQuery', true);

app.listen(3002, () =>{
    console.log("running")
})

//npm start

const { exec } = require('child_process');
const MiniZinc = require('minizinc');

app.post('/MiniML/toMiniMLAST', async (req, res) => {
  const code = req.body.code;
  console.log("code get");
  try {
    let evaluation = {
      resultat: "",
      erreur: ""
    }
    evaluation = ocaml.ml.ast(code);
    console.log("code traite");
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5174');
    res.send(JSON.stringify({ resultat: evaluation.resultat }) );
  }catch(err){
    console.log(err.message);
    res.status(400).send(JSON.stringify({ error: err.message }));
  }
});

app.post('/MiniML/toLCBPV', async (req, res) => {
  const code = req.body.code;
  console.log("code get");
  try {
    let evaluation = {
      resultat: "",
      erreur: ""
    }
    evaluation = ocaml.ml.translate(code);
    console.log("code traite");
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5174');
    res.send(JSON.stringify({ resultat: evaluation.resultat }) );
  }catch(err){
    console.log(err.message);
    res.status(400).send(JSON.stringify({ error: err.message }));
  }
});

app.post('/MiniML/toAutobillType', async (req, res) => {
  const code = req.body.code;
  console.log("code get");
  try {
    let evaluation = {
      resultat: "",
      erreur: ""
    }
    evaluation = ocaml.ml.parse(code);
    console.log("code traite");
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5174');
    res.send(JSON.stringify({ resultat: evaluation.resultat }) );
  }catch(err){
    console.log(err.message);
    res.status(400).send(JSON.stringify({ error: err.message }));
  }
});

app.post('/MiniML/toEquation', async (req, res) => {
  const code = req.body.code;
  console.log("code get");
  try {
    let evaluation = {
      resultat: "",
      erreur: ""
    }
    evaluation = ocaml.ml.mltoequation(code);
    console.log("code traite");
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5174');
    res.send(JSON.stringify({ resultat: evaluation.resultat }) );
  }catch(err){
    console.log(err.message);
    res.status(400).send(JSON.stringify({ error: err.message }));
  }
});

app.post('/MiniML/toAutobill', async (req, res) => {
  const code = req.body.code;
  console.log("code get");
  try {
    let evaluation = {
      resultat: "",
      erreur: ""
    }
    evaluation = ocaml.ml.machine(code);
    console.log("code traite");
    res.setHeader('Access-Control-Allow-Origin', 'http://localhost:5174');
    res.send(JSON.stringify({ resultat: evaluation.resultat }) );
  }catch(err){
    console.log(err.message);
    res.status(400).send(JSON.stringify({ error: err.message }));
  }

});

app.post('/minizinc/gecode', (req, res) => {
  const code = req.body.code;

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
    res.send(JSON.stringify({ resultat: stdout }) );
  });
});

