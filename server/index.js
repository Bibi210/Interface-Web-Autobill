const express = require("express");
const app = express();
const mongoose = require("mongoose");
const dotenv = require("dotenv");
const helmet = require("helmet");
const morgan = require("morgan");

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



mongoose.set('strictQuery', true);

app.listen(5174, () =>{
    console.log("running")
})

//npm start