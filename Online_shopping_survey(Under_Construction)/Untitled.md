

```R
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(forcats)
library(stringr)

options(repr.matrix.max.cols = 50)
opts <- options()
```

    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: ‘reshape’
    
    The following object is masked from ‘package:dplyr’:
    
        rename
    
    The following objects are masked from ‘package:tidyr’:
    
        expand, smiths
    



```R
duda <- read.csv('pesquisa_duda.csv', header = T)
head(duda)
```


<table>
<caption>A data.frame: 6 × 31</caption>
<thead>
	<tr><th scope=col>Start.Date</th><th scope=col>Duration..in.seconds.</th><th scope=col>Faixa.etária.</th><th scope=col>Gênero</th><th scope=col>Grau.de.escolaridade..até.que.ano.estudou..</th><th scope=col>Região.onde.mora.</th><th scope=col>Ocupação.</th><th scope=col>Faixa.de.renda.familiar..sua.renda.junto.com.a.renda.das.pessoas.que.moram.com.você..</th><th scope=col>De.forma.geral..você.gosta.de.fazer.compras.</th><th scope=col>Qual..a.maior.dificuldade.no.seu.dia.a.dia.que.torna.a.atividade.de.compra.mais.desafiadora.</th><th scope=col>Que.tipos.de.lojas..você.mais.frequenta.ou.gosta.de.frequentar...você.pode.escolher.mais.de.uma.opção....Selected.Choice</th><th scope=col>Onde.você.costuma.buscar.informação.antes.de.comprar.nas.lojas.que.você.gosta...pode.assinalar.mais.de.uma.opção....Selected.Choice</th><th scope=col>O.que.você.leva.em.consideração.na.hora.de.escolher.uma.loja.para.comprar...pode.assinalar.mais.de.uma.opção.</th><th scope=col>Como.é.a.sua.relação.com.as.lojas.onde.gosta.de.comprar.na.internet...pode.assinalar.mais.de.uma.alternativa.</th><th scope=col>Qual.o.seu.meio.preferido.para.fazer.compras....Selected.Choice</th><th scope=col>Para.você.qual.o.papel.da.internet.na.hora.de.você.fazer.uma.compra.numa.loja.</th><th scope=col>Voce.costuma.fazer.compras.online.</th><th scope=col>Que.tipos.de.produtos.você.costuma.compra.online....Selected.Choice</th><th scope=col>Você.costuma.comprar.da.mesma.loja.de.diferentes.maneiras..compra.na.loja.física.e.online.</th><th scope=col>Voce.prefere.que.as.lojas.tenham.mais.de.uma.forma.de.compra.</th><th scope=col>Considerando.comprar..produtos.de.beleza.como.maquiagem..shampoo..perfume..etc..você.prefere.comprar.de.qual.forma.</th><th scope=col>Considerando.comprar.livros..você.prefere.comprar.de.qual.forma.</th><th scope=col>Considerando.comprar.alimentos.de.hortifruti.e.supermercado..você.prefere.comprar.de.qual.forma.</th><th scope=col>Considerando.comprar.roupas.e.sapatos..você.prefere.comprar.de.qual.forma.</th><th scope=col>Considerando.comprar..eletrodomesticos.e.produtos.eletronicos..você.prefere.comprar.de.qual.forma.</th><th scope=col>Se.você.vai.até.a.loja.e.o.produto.que.você.deseja.não.esta.disponível..você.preferiria....Selected.Choice</th><th scope=col>Por.que.você.escolheria.comprar.online.e.retirar.na.loja.física.</th><th scope=col>Por.que.você.escolheria.comprar.um.produto.direto.na.loja.física.</th><th scope=col>Por.que.voce.escolheria.comprar.um.produto.online.e.receber.em.casa.</th><th scope=col>Por.que.voce.escolheria.comprar.um.produto.na.loja.fisica.e.receber.em.casa.</th><th scope=col>Oque.te.impede.de.realizar.compras.online....Selected.Choice</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>10/10/19 18:15</td><td>585</td><td>30-45 anos     </td><td>masculino</td><td>doutorado                 </td><td>Curitiba              </td><td>empregado de empresa privada    </td><td>5.000 a 10.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,supermercado                       </td><td>vitrines,pesquisando na internet                                                                            </td><td>qualidade do produto,localização da loja,confiança na marca/produto,experiencia de compra na loja                                        </td><td>estou sempre visitando o site das lojas que gosto                                                                                                                                         </td><td>loja física </td><td>pesquiso informações sobre produtos na internet</td><td>sim, faço algumas compras online             </td><td>eletrónicos                                      </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>com certeza não                                                                     </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>                                                  </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td>por poder sair com o produto na hora</td><td>por ter preço mais baixo </td><td>pela praticidade         </td><td>a inseguranca se o poduto realmente chegara                                                  </td></tr>
	<tr><td>10/13/19 11:56</td><td>249</td><td>18-29 anos     </td><td>feminino </td><td>ensino médio              </td><td>Curitiba              </td><td>estudante                       </td><td>10.000 a 15.000</td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas                                    </td><td>seguindo as lojas nas redes sociais,pesquisando na internet                                                 </td><td>qualidade do produto,promoção                                                                                                            </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais                                                                                               </td><td>site da loja</td><td>saber as novidades das lojas                   </td><td>sim, faço algumas compras online             </td><td>roupas,acessórios                                </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e ir retirar na loja                    </td><td>voltar na loja quando o produto estiver disponível</td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td>por poder experimentar o produto    </td><td>pela comodidade          </td><td>não escolheria esta opção</td><td>a incerteza se o produto ira servir,gostar da experiencia que tenho quando vou na loja física</td></tr>
	<tr><td>10/13/19 11:57</td><td>305</td><td>61 anos ou mais</td><td>masculino</td><td>ensino superior completo  </td><td>Outra cidade do Paraná</td><td>profissional liberal ou autônomo</td><td>mais de 15.000 </td><td>sim</td><td>não tenho dificuldades para fazer compras</td><td>esportes,produtos eletrônicos             </td><td>propagandas redes sociais,pesquisando na internet,sites de promoção                                         </td><td>qualidade do produto,confiança na marca/produto,experiencia de compra na loja                                                            </td><td>estou sempre visitando o site das lojas que gosto                                                                                                                                         </td><td>site da loja</td><td>faço pesquisa de preço  na internet            </td><td>sim, faço algumas compras online             </td><td>eletrónicos,passagem aérea,outros                </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>voltar na loja quando o produto estiver disponível</td><td>não escolheria esta opção                                    </td><td>por poder sair com o produto na hora</td><td>por ter preço mais baixo </td><td>pela praticidade         </td><td>a insegurança em relacao ao pagamento,nada me impede de comprar online                       </td></tr>
	<tr><td>10/13/19 11:51</td><td>707</td><td>61 anos ou mais</td><td>masculino</td><td>ensino médio              </td><td>Curitiba              </td><td>aposentado                      </td><td>5.000 a 10.000 </td><td>sim</td><td>não gosto de ir em shoppings             </td><td>supermercado                              </td><td>televisão                                                                                                   </td><td>qualidade do produto,promoção                                                                                                            </td><td>vejo propaganda nas redes sociais                                                                                                                                                         </td><td>loja física </td><td>uso a internet somente para realizar a compra  </td><td>sim, faço algumas compras online             </td><td>eletrodomêsticos                                 </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>não compro este tipo de produto                </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>comprar em outra loja                             </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>não escolheria esta opção</td><td>não escolheria esta opção</td><td>outros                                                                                       </td></tr>
	<tr><td>10/13/19 11:58</td><td>417</td><td>18-29 anos     </td><td>masculino</td><td>ensino médio              </td><td>exterior              </td><td>empregado de empresa privada    </td><td>5.000 a 10.000 </td><td>sim</td><td>não tenho dificuldades para fazer compras</td><td>roupas,sapatos,produtos eletrônicos,outros</td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,pesquisando na internet,amigos/ colegas       </td><td>qualidade do produto,produto personalizado/ exclusivo,confiança na marca/produto                                                         </td><td>estou sempre visitando o site das lojas que gosto,não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais,vejo propaganda nas redes sociais                             </td><td>loja física </td><td>pesquiso informações sobre produtos na internet</td><td>não, não gosto de comprar online             </td><td>passagem aérea,ingressos de cinema/ shows/ teatro</td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>comprar em outra loja                             </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>não escolheria esta opção</td><td>não escolheria esta opção</td><td>nada me impede de comprar online                                                             </td></tr>
	<tr><td>10/13/19 12:00</td><td>331</td><td>18-29 anos     </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>mais de 15.000 </td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas,sapatos,móveis/ decoração          </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,pesquisando na internet,WhatsApp,blogueiras/os</td><td>qualidade do produto,venda online,qualidade do atendimento,diferentes opções de entrega do produto,promoção,experiencia de compra na loja</td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais,vejo propaganda nas redes sociais,sempre entro em contato com o vendedor da loja pelo WhatsApp</td><td>site da loja</td><td>saber as novidades das lojas                   </td><td>sim, faço a maioria das minhas compras online</td><td>roupas                                           </td><td>sim, vario dependendo da minha rotina                  </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>online e receber em casa                       </td><td>não compro este tipo de produto                </td><td>não compro este tipo de produto                </td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>voltar na loja quando o produto estiver disponível</td><td>posso ver e provar                                           </td><td>por poder experimentar o produto    </td><td>pela comodidade          </td><td>não escolheria esta opção</td><td>nada me impede de comprar online                                                             </td></tr>
</tbody>
</table>




```R
colnames(duda)
```


<ol class=list-inline>
	<li>'Start.Date'</li>
	<li>'Duration..in.seconds.'</li>
	<li>'Faixa.etária.'</li>
	<li>'Gênero'</li>
	<li>'Grau.de.escolaridade..até.que.ano.estudou..'</li>
	<li>'Região.onde.mora.'</li>
	<li>'Ocupação.'</li>
	<li>'Faixa.de.renda.familiar..sua.renda.junto.com.a.renda.das.pessoas.que.moram.com.você..'</li>
	<li>'De.forma.geral..você.gosta.de.fazer.compras.'</li>
	<li>'Qual..a.maior.dificuldade.no.seu.dia.a.dia.que.torna.a.atividade.de.compra.mais.desafiadora.'</li>
	<li>'Que.tipos.de.lojas..você.mais.frequenta.ou.gosta.de.frequentar...você.pode.escolher.mais.de.uma.opção....Selected.Choice'</li>
	<li>'Onde.você.costuma.buscar.informação.antes.de.comprar.nas.lojas.que.você.gosta...pode.assinalar.mais.de.uma.opção....Selected.Choice'</li>
	<li>'O.que.você.leva.em.consideração.na.hora.de.escolher.uma.loja.para.comprar...pode.assinalar.mais.de.uma.opção.'</li>
	<li>'Como.é.a.sua.relação.com.as.lojas.onde.gosta.de.comprar.na.internet...pode.assinalar.mais.de.uma.alternativa.'</li>
	<li>'Qual.o.seu.meio.preferido.para.fazer.compras....Selected.Choice'</li>
	<li>'Para.você.qual.o.papel.da.internet.na.hora.de.você.fazer.uma.compra.numa.loja.'</li>
	<li>'Voce.costuma.fazer.compras.online.'</li>
	<li>'Que.tipos.de.produtos.você.costuma.compra.online....Selected.Choice'</li>
	<li>'Você.costuma.comprar.da.mesma.loja.de.diferentes.maneiras..compra.na.loja.física.e.online.'</li>
	<li>'Voce.prefere.que.as.lojas.tenham.mais.de.uma.forma.de.compra.'</li>
	<li>'Considerando.comprar..produtos.de.beleza.como.maquiagem..shampoo..perfume..etc..você.prefere.comprar.de.qual.forma.'</li>
	<li>'Considerando.comprar.livros..você.prefere.comprar.de.qual.forma.'</li>
	<li>'Considerando.comprar.alimentos.de.hortifruti.e.supermercado..você.prefere.comprar.de.qual.forma.'</li>
	<li>'Considerando.comprar.roupas.e.sapatos..você.prefere.comprar.de.qual.forma.'</li>
	<li>'Considerando.comprar..eletrodomesticos.e.produtos.eletronicos..você.prefere.comprar.de.qual.forma.'</li>
	<li>'Se.você.vai.até.a.loja.e.o.produto.que.você.deseja.não.esta.disponível..você.preferiria....Selected.Choice'</li>
	<li>'Por.que.você.escolheria.comprar.online.e.retirar.na.loja.física.'</li>
	<li>'Por.que.você.escolheria.comprar.um.produto.direto.na.loja.física.'</li>
	<li>'Por.que.voce.escolheria.comprar.um.produto.online.e.receber.em.casa.'</li>
	<li>'Por.que.voce.escolheria.comprar.um.produto.na.loja.fisica.e.receber.em.casa.'</li>
	<li>'Oque.te.impede.de.realizar.compras.online....Selected.Choice'</li>
</ol>




```R
colnames(duda) <- c('date', 'duration', 'faixa_etaria', 'genero', 'escolaridade', 'regiao', 'ocupacao', 'renda_familiar',
                   'gosta_compras', 'dificuldade_compra', 'tipo_loja', 'fonte_informacao', 'consideracao_loja', 'relacao_loja_online',
                    'meio_preferido', 'papel_internet', 'faz_compra_online', 'tipos_prod_online', 'mesma_loja_diferente_maneira', 'gosta_mais_meio_compra',
                   'beleza_meio_compra', 'livro_meio_compra', 'alimento_meio_compra', 'vestuario_meio_compra', 'eletrodomestico_meio_compra', 'produto_nao_disp_acao',
                    'pq_compra_online_retira_fisico', 'pq_compra_fisica', 'pq_compra_online_recebe_casa', 'pq_compra_fisica_recebe_casa', 'impedimento_compra_online')

duda$date <- as.POSIXct(strptime(duda$date, format="%m/%d/%y %H:%M"))

duda <- cbind(seq(1:nrow(duda)), duda)
colnames(duda)[1] <- 'ID'
head(duda, 20)
```


<table>
<caption>A data.frame: 20 × 32</caption>
<thead>
	<tr><th scope=col>ID</th><th scope=col>date</th><th scope=col>duration</th><th scope=col>faixa_etaria</th><th scope=col>genero</th><th scope=col>escolaridade</th><th scope=col>regiao</th><th scope=col>ocupacao</th><th scope=col>renda_familiar</th><th scope=col>gosta_compras</th><th scope=col>dificuldade_compra</th><th scope=col>tipo_loja</th><th scope=col>fonte_informacao</th><th scope=col>consideracao_loja</th><th scope=col>relacao_loja_online</th><th scope=col>meio_preferido</th><th scope=col>papel_internet</th><th scope=col>faz_compra_online</th><th scope=col>tipos_prod_online</th><th scope=col>mesma_loja_diferente_maneira</th><th scope=col>gosta_mais_meio_compra</th><th scope=col>beleza_meio_compra</th><th scope=col>livro_meio_compra</th><th scope=col>alimento_meio_compra</th><th scope=col>vestuario_meio_compra</th><th scope=col>eletrodomestico_meio_compra</th><th scope=col>produto_nao_disp_acao</th><th scope=col>pq_compra_online_retira_fisico</th><th scope=col>pq_compra_fisica</th><th scope=col>pq_compra_online_recebe_casa</th><th scope=col>pq_compra_fisica_recebe_casa</th><th scope=col>impedimento_compra_online</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dttm&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td> 1</td><td>2019-10-10 18:15:00</td><td>585</td><td>30-45 anos     </td><td>masculino</td><td>doutorado                 </td><td>Curitiba              </td><td>empregado de empresa privada    </td><td>5.000 a 10.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,supermercado                          </td><td>vitrines,pesquisando na internet                                                                                                                                                                     </td><td>qualidade do produto,localização da loja,confiança na marca/produto,experiencia de compra na loja                                                                                                                                                                                                 </td><td>estou sempre visitando o site das lojas que gosto                                                                                                                                                                                    </td><td>loja física </td><td>pesquiso informações sobre produtos na internet  </td><td>sim, faço algumas compras online             </td><td>eletrónicos                                                                                                                        </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>com certeza não                                                                     </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>                                                  </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td>por poder sair com o produto na hora</td><td>por ter preço mais baixo         </td><td>pela praticidade                                    </td><td>a inseguranca se o poduto realmente chegara                                                                                                                                                        </td></tr>
	<tr><td> 2</td><td>2019-10-13 11:56:00</td><td>249</td><td>18-29 anos     </td><td>feminino </td><td>ensino médio              </td><td>Curitiba              </td><td>estudante                       </td><td>10.000 a 15.000</td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas                                       </td><td>seguindo as lojas nas redes sociais,pesquisando na internet                                                                                                                                          </td><td>qualidade do produto,promoção                                                                                                                                                                                                                                                                     </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais                                                                                                                                          </td><td>site da loja</td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,acessórios                                                                                                                  </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e ir retirar na loja                    </td><td>voltar na loja quando o produto estiver disponível</td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>não escolheria esta opção                           </td><td>a incerteza se o produto ira servir,gostar da experiencia que tenho quando vou na loja física                                                                                                      </td></tr>
	<tr><td> 3</td><td>2019-10-13 11:57:00</td><td>305</td><td>61 anos ou mais</td><td>masculino</td><td>ensino superior completo  </td><td>Outra cidade do Paraná</td><td>profissional liberal ou autônomo</td><td>mais de 15.000 </td><td>sim</td><td>não tenho dificuldades para fazer compras</td><td>esportes,produtos eletrônicos                </td><td>propagandas redes sociais,pesquisando na internet,sites de promoção                                                                                                                                  </td><td>qualidade do produto,confiança na marca/produto,experiencia de compra na loja                                                                                                                                                                                                                     </td><td>estou sempre visitando o site das lojas que gosto                                                                                                                                                                                    </td><td>site da loja</td><td>faço pesquisa de preço  na internet              </td><td>sim, faço algumas compras online             </td><td>eletrónicos,passagem aérea,outros                                                                                                  </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>voltar na loja quando o produto estiver disponível</td><td>não escolheria esta opção                                    </td><td>por poder sair com o produto na hora</td><td>por ter preço mais baixo         </td><td>pela praticidade                                    </td><td>a insegurança em relacao ao pagamento,nada me impede de comprar online                                                                                                                             </td></tr>
	<tr><td> 4</td><td>2019-10-13 11:51:00</td><td>707</td><td>61 anos ou mais</td><td>masculino</td><td>ensino médio              </td><td>Curitiba              </td><td>aposentado                      </td><td>5.000 a 10.000 </td><td>sim</td><td>não gosto de ir em shoppings             </td><td>supermercado                                 </td><td>televisão                                                                                                                                                                                            </td><td>qualidade do produto,promoção                                                                                                                                                                                                                                                                     </td><td>vejo propaganda nas redes sociais                                                                                                                                                                                                    </td><td>loja física </td><td>uso a internet somente para realizar a compra    </td><td>sim, faço algumas compras online             </td><td>eletrodomêsticos                                                                                                                   </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>não compro este tipo de produto                </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>comprar em outra loja                             </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>não escolheria esta opção        </td><td>não escolheria esta opção                           </td><td>outros                                                                                                                                                                                             </td></tr>
	<tr><td> 5</td><td>2019-10-13 11:58:00</td><td>417</td><td>18-29 anos     </td><td>masculino</td><td>ensino médio              </td><td>exterior              </td><td>empregado de empresa privada    </td><td>5.000 a 10.000 </td><td>sim</td><td>não tenho dificuldades para fazer compras</td><td>roupas,sapatos,produtos eletrônicos,outros   </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,pesquisando na internet,amigos/ colegas                                                                                                </td><td>qualidade do produto,produto personalizado/ exclusivo,confiança na marca/produto                                                                                                                                                                                                                  </td><td>estou sempre visitando o site das lojas que gosto,não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais,vejo propaganda nas redes sociais                                                                        </td><td>loja física </td><td>pesquiso informações sobre produtos na internet  </td><td>não, não gosto de comprar online             </td><td>passagem aérea,ingressos de cinema/ shows/ teatro                                                                                  </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>comprar em outra loja                             </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>não escolheria esta opção        </td><td>não escolheria esta opção                           </td><td>nada me impede de comprar online                                                                                                                                                                   </td></tr>
	<tr><td> 6</td><td>2019-10-13 12:00:00</td><td>331</td><td>18-29 anos     </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>mais de 15.000 </td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas,sapatos,móveis/ decoração             </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,pesquisando na internet,WhatsApp,blogueiras/os                                                                                         </td><td>qualidade do produto,venda online,qualidade do atendimento,diferentes opções de entrega do produto,promoção,experiencia de compra na loja                                                                                                                                                         </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais,vejo propaganda nas redes sociais,sempre entro em contato com o vendedor da loja pelo WhatsApp                                           </td><td>site da loja</td><td>saber as novidades das lojas                     </td><td>sim, faço a maioria das minhas compras online</td><td>roupas                                                                                                                             </td><td>sim, vario dependendo da minha rotina                  </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>online e receber em casa                       </td><td>não compro este tipo de produto                </td><td>não compro este tipo de produto                </td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>voltar na loja quando o produto estiver disponível</td><td>posso ver e provar                                           </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>não escolheria esta opção                           </td><td>nada me impede de comprar online                                                                                                                                                                   </td></tr>
	<tr><td> 7</td><td>2019-10-13 11:59:00</td><td>422</td><td>18-29 anos     </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>5.000 a 10.000 </td><td>sim</td><td>não tenho dificuldades para fazer compras</td><td>roupas                                       </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,amigos/ colegas                                                                                                                        </td><td>qualidade do produto,qualidade do atendimento,confiança na marca/produto                                                                                                                                                                                                                          </td><td>estou sempre visitando o site das lojas que gosto                                                                                                                                                                                    </td><td>site da loja</td><td>pesquiso informações sobre produtos na internet  </td><td>sim, faço algumas compras online             </td><td>roupas,acessórios,sapato,produtos de beleza,passagem aérea                                                                         </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>realizar a compra na loja e receber na sua casa   </td><td>não escolheria esta opção                                    </td><td>gosto de ver e sentir o produto     </td><td>pela comodidade                  </td><td>por nao precisar carregar os produtos               </td><td>a insegurança em relacao ao pagamento,a inseguranca se o poduto realmente chegara,não poder experimentar o produto                                                                                 </td></tr>
	<tr><td> 8</td><td>2019-10-13 11:59:00</td><td>457</td><td>18-29 anos     </td><td>feminino </td><td>pós graduação,            </td><td>Curitiba              </td><td>profissional liberal ou autônomo</td><td>5.000 a 10.000 </td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas,sapatos,livrarias                     </td><td>vitrines,propagandas em revista/jornal,propagandas redes sociais,seguindo as lojas nas redes sociais,pesquisando na internet,sites de promoção,WhatsApp,amigos/ colegas,blogueiras/os,email marketing</td><td>qualidade do produto,presença nas mídias sociais,informações sobre o produto,localização da loja,produto personalizado/ exclusivo,venda online,qualidade do atendimento,padronização do produto em relação a numeração,diferentes opções de entrega do produto,confiança na marca/produto,promoção</td><td>sigo as lojas que gosto nas redes sociais                                                                                                                                                                                            </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,sapato,produtos de beleza,livros,passagem aérea,ingressos de cinema/ shows/ teatro                                          </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e receber na sua casa           </td><td>comprar em outra loja                             </td><td>não tem custo adicional de frete                             </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>não escolheria esta opção                           </td><td>não poder experimentar o produto,a demora para receber o produto                                                                                                                                   </td></tr>
	<tr><td> 9</td><td>2019-10-13 12:02:00</td><td>296</td><td>30-45 anos     </td><td>feminino </td><td>ensino superior completo  </td><td>Curitiba              </td><td>empregado de empresa privada    </td><td>mais de 15.000 </td><td>sim</td><td>falta de estacionamento nos locais       </td><td>roupas,supermercado                          </td><td>seguindo as lojas nas redes sociais                                                                                                                                                                  </td><td>experiencia de compra na loja                                                                                                                                                                                                                                                                     </td><td>sigo as lojas que gosto nas redes sociais                                                                                                                                                                                            </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>filmes,passagem aérea,ingressos de cinema/ shows/ teatro                                                                           </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e receber na sua casa           </td><td>comprar em outra loja                             </td><td>não escolheria esta opção                                    </td><td>gosto de ver e sentir o produto     </td><td>pela comodidade                  </td><td>não escolheria esta opção                           </td><td>a incerteza se o produto ira servir                                                                                                                                                                </td></tr>
	<tr><td>10</td><td>2019-10-13 12:02:00</td><td>299</td><td>18-29 anos     </td><td>feminino </td><td>pós graduação,            </td><td>Curitiba              </td><td>profissional liberal ou autônomo</td><td>2.000 a 5.000  </td><td>não</td><td>falta de tempo                           </td><td>roupas,sapatos,produtos eletrônicos,livrarias</td><td>vitrines,propagandas redes sociais                                                                                                                                                                   </td><td>qualidade do produto,produto personalizado/ exclusivo,qualidade do atendimento,confiança na marca/produto,experiencia de compra na loja                                                                                                                                                           </td><td>sigo as lojas que gosto nas redes sociais                                                                                                                                                                                            </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>não, não gosto de comprar online             </td><td>livros,passagem aérea,ingressos de cinema/ shows/ teatro                                                                           </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>voltar na loja quando o produto estiver disponível</td><td>não tem custo adicional de frete                             </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>não escolheria esta opção                           </td><td>a insegurança em relacao ao pagamento,gostar da experiencia que tenho quando vou na loja física,não poder experimentar o produto,a falta de padronização dos tamanhos                              </td></tr>
	<tr><td>11</td><td>2019-10-13 12:03:00</td><td>315</td><td>18-29 anos     </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>mais de 15.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,sapatos                               </td><td>seguindo as lojas nas redes sociais,amigos/ colegas,blogueiras/os                                                                                                                                    </td><td>qualidade do produto,promoção,experiencia de compra na loja                                                                                                                                                                                                                                       </td><td>estou sempre visitando o site das lojas que gosto                                                                                                                                                                                    </td><td>loja física </td><td>pesquiso informações sobre produtos na internet  </td><td>sim, faço algumas compras online             </td><td>roupas,sapato                                                                                                                      </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>realizar a compra na loja e receber na sua casa   </td><td>não tem custo adicional de frete                             </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>pela praticidade                                    </td><td>não poder experimentar o produto                                                                                                                                                                   </td></tr>
	<tr><td>12</td><td>2019-10-13 12:02:00</td><td>446</td><td>30-45 anos     </td><td>feminino </td><td>ensino superior completo  </td><td>Curitiba              </td><td>empregado de empresa privada    </td><td>10.000 a 15.000</td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas,sapatos,supermercado                  </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,blogueiras/os                                                                                                                          </td><td>qualidade do produto,presença nas mídias sociais,venda online,padronização do produto em relação a numeração,diferentes opções de entrega do produto,confiança na marca/produto,promoção                                                                                                          </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais,vejo propaganda nas redes sociais                                                                                                        </td><td>site da loja</td><td>utilizo a internet em todo meu processo de compra</td><td>sim, faço a maioria das minhas compras online</td><td>roupas,acessórios,sapato,produtos de beleza,livros,passagem aérea                                                                  </td><td>sim, vario dependendo da minha rotina                  </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>na loja física e receber na sua casa           </td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>realizar a compra na loja e receber na sua casa   </td><td>não tem custo adicional de frete                             </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>por não precisa voltar na loja para buscar o produto</td><td>a incerteza se o produto ira servir,não poder experimentar o produto,não poder ver e tocar o produto                                                                                               </td></tr>
	<tr><td>13</td><td>2019-10-13 12:05:00</td><td>302</td><td>18-29 anos     </td><td>feminino </td><td>pós graduação,            </td><td>Curitiba              </td><td>empregado de empresa privada    </td><td>5.000 a 10.000 </td><td>sim</td><td>tenho preguiça de ir até as lojas        </td><td>roupas,sapatos                               </td><td>pesquisando na internet                                                                                                                                                                              </td><td>qualidade do produto                                                                                                                                                                                                                                                                              </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais                                                                                                                                          </td><td>site da loja</td><td>utilizo a internet em todo meu processo de compra</td><td>sim, faço a maioria das minhas compras online</td><td>roupas,acessórios,sapato,fármacia,eletrónicos,eletrodomêsticos,produtos de beleza,passagem aérea,ingressos de cinema/ shows/ teatro</td><td>não, costuma fazer minhas compras sempre online        </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>desistir de comprar                               </td><td>não escolheria esta opção                                    </td><td>não escolheria esta opção           </td><td>pela comodidade                  </td><td>não escolheria esta opção                           </td><td>nada me impede de comprar online                                                                                                                                                                   </td></tr>
	<tr><td>14</td><td>2019-10-13 12:06:00</td><td>282</td><td>18-29 anos     </td><td>feminino </td><td>pós graduação,            </td><td>Curitiba              </td><td>empregado de empresa privada    </td><td>mais de 15.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,sapatos                               </td><td>propagandas redes sociais,blogueiras/os,email marketing                                                                                                                                              </td><td>qualidade do produto                                                                                                                                                                                                                                                                              </td><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais,abro os emails marketing que recebo das lojas que gosto,tenho o aplicativo das lojas que gosto,sempre entro em contato com o vendedor da loja pelo WhatsApp</td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,sapato,produtos de beleza                                                                                                   </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>não compro este tipo de produto                </td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>realizar a compra na loja e receber na sua casa   </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>pela praticidade                                    </td><td>a incerteza se o produto ira servir,não poder experimentar o produto,não poder ver e tocar o produto,a falta de padronização dos tamanhos                                                          </td></tr>
	<tr><td>15</td><td>2019-10-13 12:07:00</td><td>241</td><td>até 17 anos    </td><td>masculino</td><td>ensino médio              </td><td>exterior              </td><td>estudante                       </td><td>mais de 15.000 </td><td>sim</td><td>não tenho dificuldades para fazer compras</td><td>roupas,sapatos,esportes,produtos eletrônicos </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,WhatsApp,amigos/ colegas                                                                                                               </td><td>qualidade do produto,presença nas mídias sociais,informações sobre o produto,qualidade do atendimento,confiança na marca/produto,promoção                                                                                                                                                         </td><td>vejo propaganda nas redes sociais,tenho o aplicativo das lojas que gosto                                                                                                                                                             </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,acessórios,sapato,eletrónicos,passagem aérea,ingressos de cinema/ shows/ teatro                                             </td><td>sim, vario dependendo da minha rotina                  </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>realizar a compra na loja e receber na sua casa   </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>por só ter certos produtos online</td><td>por nao precisar carregar os produtos               </td><td>nada me impede de comprar online                                                                                                                                                                   </td></tr>
	<tr><td>16</td><td>2019-10-13 12:06:00</td><td>305</td><td>18-29 anos     </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>10.000 a 15.000</td><td>sim</td><td>falta de tempo                           </td><td>roupas,sapatos                               </td><td>seguindo as lojas nas redes sociais,pesquisando na internet,blogueiras/os                                                                                                                            </td><td>qualidade do produto,venda online,padronização do produto em relação a numeração,experiencia de compra na loja                                                                                                                                                                                    </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais                                                                                                                                          </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,acessórios,eletrónicos,produtos de beleza,passagem aérea,ingressos de cinema/ shows/ teatro                                 </td><td>sim, vario dependendo da minha rotina                  </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>comprar em outra loja                             </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td>gosto de ver e sentir o produto     </td><td>pela comodidade                  </td><td>por nao precisar carregar os produtos               </td><td>a incerteza se o produto ira servir,gostar da experiencia que tenho quando vou na loja física,não poder experimentar o produto,a demora para receber o produto,a falta de padronização dos tamanhos</td></tr>
	<tr><td>17</td><td>2019-10-13 12:07:00</td><td>282</td><td>até 17 anos    </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>5.000 a 10.000 </td><td>sim</td><td>não gosto de ter contato com vendedores  </td><td>roupas,sapatos                               </td><td>seguindo as lojas nas redes sociais                                                                                                                                                                  </td><td>qualidade do produto,informações sobre o produto,confiança na marca/produto,promoção                                                                                                                                                                                                              </td><td>sigo as lojas que gosto nas redes sociais                                                                                                                                                                                            </td><td>loja física </td><td>não uso a internet antes de fazer uma compra     </td><td>não, não gosto de comprar online             </td><td>não compro online                                                                                                                  </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>desistir de comprar                               </td><td>não escolheria esta opção                                    </td><td>não escolheria esta opção           </td><td>não escolheria esta opção        </td><td>não escolheria esta opção                           </td><td>gostar da experiencia que tenho quando vou na loja física,não poder experimentar o produto,não poder ver e tocar o produto,a demora para receber o produto,a falta de padronização dos tamanhos    </td></tr>
	<tr><td>18</td><td>2019-10-13 12:05:00</td><td>426</td><td>até 17 anos    </td><td>feminino </td><td>ensino médio              </td><td>Curitiba              </td><td>estudante                       </td><td>mais de 15.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,sapatos                               </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,pesquisando na internet,blogueiras/os                                                                                                  </td><td>qualidade do produto,presença nas mídias sociais,localização da loja,produto personalizado/ exclusivo,venda online                                                                                                                                                                                </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais                                                                                                                                          </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,sapato,produtos de beleza,passagem aérea,ingressos de cinema/ shows/ teatro                                                 </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>online e receber em casa                       </td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>realizar a compra na loja e receber na sua casa   </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>pela praticidade                                    </td><td>a incerteza se o produto ira servir,a demora para receber o produto                                                                                                                                </td></tr>
	<tr><td>19</td><td>2019-10-13 12:07:00</td><td>340</td><td>18-29 anos     </td><td>feminino </td><td>ensino superior incompleto</td><td>Curitiba              </td><td>estudante                       </td><td>mais de 15.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,sapatos,livrarias,outros              </td><td>vitrines,propagandas redes sociais,seguindo as lojas nas redes sociais,blogueiras/os                                                                                                                 </td><td>qualidade do produto,localização da loja,qualidade do atendimento,experiencia de compra na loja                                                                                                                                                                                                   </td><td>estou sempre visitando o site das lojas que gosto,sigo as lojas que gosto nas redes sociais,tenho o aplicativo das lojas que gosto                                                                                                   </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>acessórios,produtos de beleza,livros,passagem aérea,ingressos de cinema/ shows/ teatro                                             </td><td>sim, vario dependendo da minha rotina                  </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>online e receber em casa                       </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e receber na sua casa           </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>realizar a compra na loja e receber na sua casa   </td><td>não escolheria esta opção                                    </td><td>por poder experimentar o produto    </td><td>pela comodidade                  </td><td>pela praticidade                                    </td><td>a incerteza se o produto ira servir,a inseguranca se o poduto realmente chegara                                                                                                                    </td></tr>
	<tr><td>20</td><td>2019-10-13 12:11:00</td><td>345</td><td>46-60 anos     </td><td>feminino </td><td>pós graduação,            </td><td>Curitiba              </td><td>funcionário publico             </td><td>mais de 15.000 </td><td>sim</td><td>falta de tempo                           </td><td>roupas,sapatos,supermercado                  </td><td>propagandas redes sociais,seguindo as lojas nas redes sociais,sites de promoção,WhatsApp                                                                                                             </td><td>qualidade do produto,presença nas mídias sociais,qualidade do atendimento,confiança na marca/produto,promoção                                                                                                                                                                                     </td><td>sigo as lojas que gosto nas redes sociais,sempre entro em contato com o vendedor da loja pelo WhatsApp                                                                                                                               </td><td>loja física </td><td>saber as novidades das lojas                     </td><td>sim, faço algumas compras online             </td><td>roupas,eletrodomêsticos                                                                                                            </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>indiferente                                                                         </td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>na loja física e levar embora na hora da compra</td><td>online e receber em casa                       </td><td>comprar em outra loja                             </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td>gosto de ver e sentir o produto     </td><td>pela comodidade                  </td><td>por nao precisar carregar os produtos               </td><td>a incerteza se o produto ira servir,não poder experimentar o produto,não poder ver e tocar o produto                                                                                               </td></tr>
</tbody>
</table>




```R
multi_cols <- c('tipo_loja', 'fonte_informacao', 'consideracao_loja', 'relacao_loja_online', 'tipos_prod_online', 'impedimento_compra_online')
demo_cols <- c('faixa_etaria', 'genero', 'escolaridade', 'regiao', 'ocupacao', 'renda_familiar')
```


```R
max_unique_multi <- max(unlist(lapply(multi_cols, function(x){
        uniques <- apply(data.frame(do.call(rbind, strsplit(as.character(duda[,x]), ','))), 1, unique)
        max(unlist(lapply(uniques, length)))
})))
```

    Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 1)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 3)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 8)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 6)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 7)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 7)”


```R
multi_df_list <- list()
i <- 1

for (col in multi_cols){
    unique_type <- data.frame(do.call(rbind, apply(data.frame(do.call(rbind, strsplit(as.character(duda[, col]), ','))), 1, unique)))
    colnames(unique_type) <- c(sprintf(paste(col,'.%s'),seq(1:(ncol(unique_type)))))
    multi_df_list[[i]] <- unique_type
    i <- i + 1
    }

head(multi_df_list[[1]])
```

    Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 1)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 1)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 3)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 3)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 8)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 8)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 6)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 6)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 7)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 7)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 7)”Warning message in (function (..., deparse.level = 1) :
    “number of columns of result is not a multiple of vector length (arg 7)”


<table>
<caption>A data.frame: 6 × 7</caption>
<thead>
	<tr><th scope=col>tipo_loja .1</th><th scope=col>tipo_loja .2</th><th scope=col>tipo_loja .3</th><th scope=col>tipo_loja .4</th><th scope=col>tipo_loja .5</th><th scope=col>tipo_loja .6</th><th scope=col>tipo_loja .7</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>roupas      </td><td>supermercado        </td><td>roupas              </td><td>supermercado        </td><td>roupas      </td><td>supermercado        </td><td>roupas              </td></tr>
	<tr><td>roupas      </td><td>roupas              </td><td>roupas              </td><td>roupas              </td><td>roupas      </td><td>roupas              </td><td>roupas              </td></tr>
	<tr><td>esportes    </td><td>produtos eletrônicos</td><td>esportes            </td><td>produtos eletrônicos</td><td>esportes    </td><td>produtos eletrônicos</td><td>esportes            </td></tr>
	<tr><td>supermercado</td><td>supermercado        </td><td>supermercado        </td><td>supermercado        </td><td>supermercado</td><td>supermercado        </td><td>supermercado        </td></tr>
	<tr><td>roupas      </td><td>sapatos             </td><td>produtos eletrônicos</td><td>outros              </td><td>roupas      </td><td>sapatos             </td><td>produtos eletrônicos</td></tr>
	<tr><td>roupas      </td><td>sapatos             </td><td>móveis/ decoração   </td><td>roupas              </td><td>sapatos     </td><td>móveis/ decoração   </td><td>roupas              </td></tr>
</tbody>
</table>




```R
multi_df_list <- lapply(multi_df_list, function(df){
    
    name <- substr(colnames(df)[[1]],  1, nchar(colnames(df)[[1]]) - 3)
    new_df <- data.frame(t(apply(df, 1, function(row){
        l <- length(row)
        c(unique(row), rep(NA, l - length(unique(row))))
    })))
    colnames(new_df) <- c(sprintf(paste(name,'.%s'),seq(1:(ncol(new_df)))))
    return (new_df)
})
```


```R
multi_df <- do.call(cbind, multi_df_list)
multi_df <- cbind(seq(1:nrow(multi_df)), multi_df)
colnames(multi_df)[1] <- 'ID'

```


```R
multi_df_long <- arrange(gather(multi_df, key, value, -ID), ID)
```

    Warning message:
    “attributes are not identical across measure variables;
    they will be dropped”


```R
head(multi_df_long, 20)
```


<table>
<caption>A data.frame: 20 × 3</caption>
<thead>
	<tr><th scope=col>ID</th><th scope=col>key</th><th scope=col>value</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1</td><td>tipo_loja .1        </td><td>roupas                    </td></tr>
	<tr><td>1</td><td>tipo_loja .2        </td><td>supermercado              </td></tr>
	<tr><td>1</td><td>tipo_loja .3        </td><td>NA                        </td></tr>
	<tr><td>1</td><td>tipo_loja .4        </td><td>NA                        </td></tr>
	<tr><td>1</td><td>tipo_loja .5        </td><td>NA                        </td></tr>
	<tr><td>1</td><td>tipo_loja .6        </td><td>NA                        </td></tr>
	<tr><td>1</td><td>tipo_loja .7        </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .1 </td><td>vitrines                  </td></tr>
	<tr><td>1</td><td>fonte_informacao .2 </td><td>pesquisando na internet   </td></tr>
	<tr><td>1</td><td>fonte_informacao .3 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .4 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .5 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .6 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .7 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .8 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .9 </td><td>NA                        </td></tr>
	<tr><td>1</td><td>fonte_informacao .10</td><td>NA                        </td></tr>
	<tr><td>1</td><td>consideracao_loja .1</td><td>qualidade do produto      </td></tr>
	<tr><td>1</td><td>consideracao_loja .2</td><td>localização da loja       </td></tr>
	<tr><td>1</td><td>consideracao_loja .3</td><td>confiança na marca/produto</td></tr>
</tbody>
</table>




```R
multi_df_long_join <- left_join(multi_df_long, duda, by = 'ID')
dim(duda)
dim(multi_df_long_join)
```


<ol class=list-inline>
	<li>264</li>
	<li>32</li>
</ol>




<ol class=list-inline>
	<li>14520</li>
	<li>34</li>
</ol>




```R
labeldict <- list(faixa_etaria = 'Faixa Etária', genero = 'Gênero', escolaridade = 'Escolaridade', regiao = 'Região', ocupacao = 'Ocupação', renda_familiar = 'Renda Familiar',
                   gosta_compras = 'Gosta de fazer compras?', dificuldade_compra = 'Maior dificuldade de comprar online', tipo_loja = 'Tipo de loja favorita', fonte_informacao = 'Fonte de informação',
                  consideracao_loja = 'O que leva em considereção na hora de escolher uma loja?', relacao_loja_online = 'Como é sua relação com suas lojas preferidas?',
                    meio_preferido = 'Meio de compra preferido', papel_internet = 'Qual o papel da internet na hora de fazer compras?', faz_compra_online = 'Costuma comprar online?',
                  tipos_prod_online = 'Que tipos de produtos costuma comprar online?', mesma_loja_diferente_maneira = 'Compra na mesma loja de diferentes maneiras?', 
                  gosta_mais_meio_compra = 'Prefere que as lojas tenham mais de um meio de compra?',beleza_meio_compra = 'Meio de compra favorito (Produtos de Beleza)',
                  livro_meio_compra = 'Meio de compra favorito (Livros)', alimento_meio_compra = 'MEio de compra favorito (Alimentação)', vestuario_meio_compra = 'Meio de compra favorito (Vestuário)',
                  eletrodomestico_meio_compra = 'Meio de compra favorito (Eletrodomésticos)', produto_nao_disp_acao = 'Qual sua ação ao nao encontrar disponibilidade do produto?',
                    pq_compra_online_retira_fisico = 'Por que compraria online para retirar na loja física?', pq_compra_fisica = 'Por que compraria em loja física?', 
                  pq_compra_online_recebe_casa = 'Por que compraria online para receber em casa?', pq_compra_fisica_recebe_casa = 'Por que compraria em loja física para receber em casa?',
                  impedimento_compra_online = 'Qual seu maior impedimento ao comprar em lojas online?')
```


```R
plot_duda <- function(col, reorder = F, levels = F, title = '', ylab = 'Quantidade', xlab = '', legend_lab = '', width = 7, height = 7){
    
    options(repr.plot.width = width, repr.plot.height = height)
    
    tbl <- data.frame(with(duda, table(eval(parse(text = col)))))
    tbl$pct <- round((tbl$Freq / sum(tbl$Freq))*100, 2) 
    tbl <- tbl[!(tbl[,1]==""), ]
    
    if (reorder == T){
        
        tbl$Var1 <- fct_rev(fct_reorder(tbl$Var1, tbl$Freq))
        }
    
    if (levels == T){
        
        tbl$Var1 <- factor(tbl$Var1, levels = levels)
    }
    
    

    print(ggplot(tbl, aes(x = Var1, y = Freq, fill = Var1)) + 
        geom_col(position = 'identity') +
        ylab(ylab) + 
        xlab(xlab) +
        labs(fill = legend_lab) +
        ggtitle(title) +
        theme(axis.text.x = element_blank()) +
        geom_text(aes(label = paste(tbl$pct,'%')), position = position_dodge(width = .9), vjust = -.5))
    return(tbl)

}
```


```R
plot_multi <- function(col, reorder = F, levels = F, title = '', ylab = 'Quantidade', xlab = '', legend_lab = '', width = 7, height = 7){
    
    options(repr.plot.width = width, repr.plot.height = height)
    
    temp_df <- na.exclude(subset(multi_df_long, startsWith(multi_df_long$key, col)))
    
    tbl <- data.frame(with(temp_df, table(value)))
    tbl$pct <- round((tbl$Freq / sum(tbl$Freq)) * 100,2)
    tbl <- tbl[!(tbl[,1]==""), ]
    
    if (reorder == T){
        
        tbl$value <- fct_rev(fct_reorder(tbl$value, tbl$Freq))
        }
    
    if (levels == T){
        
        tbl$value <- factor(tbl$value, levels = levels)
    }
    
    

    print(ggplot(tbl, aes(x = value, y = Freq, fill = value)) + 
        geom_col(position = 'identity') +
        ylab(ylab) + 
        xlab(xlab) +
        labs(fill = legend_lab) +
        ggtitle(title) +
        theme(axis.text.x = element_blank()) +
        geom_text(aes(label = paste(tbl$pct,'%')), position = position_dodge(width = .9), vjust = -.5))
    return(tbl)

}
```


```R
plot_relation <- function(var1, var2,  reorder = F, levels = F, title = '', ylab = 'Quantidade', xlab = '', legend_lab = '', width = 7, height = 7,
                          nas = F, nudgex1 = 0, nudgey1 = 0, nudgex2 = 0, nudgey2 = 0, cex = 2.8){
    
    options(repr.plot.width = width, repr.plot.height = height)
    
    tbl <- data.frame(with(duda, table(eval(parse(text = var1)), eval(parse(text = var2)))))
    tbl$pct <- round((tbl$Freq / sum(tbl$Freq))*100, 2) 
    tbl <- tbl[!(tbl[,1]==""), ]
    tbl$Var2 <- fct_reorder(tbl$Var2, tbl$Freq)
    
    ix <- which(tbl[,2] == '')
    if (nas == T){
        tbl <- tbl[-ix,]
    }
    if (reorder == T){
        
        tbl$value <- fct_rev(fct_reorder(tbl$value, tbl$Freq))
        }
    
    if (levels == T){
        
        tbl$value <- factor(tbl$value, levels = levels)
    }
    
   
    
    p1 <- ggplot(tbl, aes(x = Var1, y = Freq, fill = Var2)) + 
    geom_col() + 
    xlab(labeldict[[var1]]) +
    labs(fill = legend_lab) +
    ggtitle(title)
    
    p2 <- ggplot(tbl, aes(x = Var1, y = Freq, fill = Var2)) + 
        geom_col(position = 'fill') +
        ylab('Proporção') + 
        xlab(xlab) +
        labs(fill = legend_lab) +
        ggtitle(title)
    

        


    temp_df <- data.frame(with(duda, table(eval(parse(text = var1)), eval(parse(text = var2)))))
    temp_df <- lapply(split(temp_df, temp_df[,1]), function(x){data.frame(as.character(x[,2]),x$Freq / sum(x$Freq))})
    #return(temp_df)
    temp_df <- do.call(rbind, temp_df)
    #return(temp_df)
    temp_df$cat <- rownames(temp_df) 
    colnames(temp_df) <- c(var2, 'Proporcao', var1)
    rownames(temp_df) <- NULL
    temp_df$Proporcao <- round(temp_df$Proporcao * 100, 1)
    temp_df[,3] <- substr(temp_df[,3], 1, nchar(temp_df[,3])-2)
    temp_df[,3] <- factor(as.character(temp_df[,3]), levels = levels(tbl$Var1))
    #return(list(tbl, temp_df))




    p3 <- ggplot(temp_df, aes_string(x = var1, y = var2, size = 'Proporcao', col = var1)) +
        geom_point() +
        theme(axis.text.x = element_blank(),
             legend.title = element_blank()) +
        scale_size(range = c(.1, 24)) +
        geom_text(aes(label = paste(temp_df$Proporcao,'%')), colour = 'black', position = position_nudge(nudgex1, nudgey1), cex = cex, show.legend = FALSE) +
        ylab(labeldict[[var2]]) +
        xlab(labeldict[[var1]]) +
        guides(size=FALSE)

    
    
    temp_df2 <- data.frame(with(duda, table(eval(parse(text = var1)), eval(parse(text = var2)))))
    temp_df2 <- lapply(split(temp_df2, temp_df2[,2]), function(x){data.frame(as.character(x[,1]),x$Freq / sum(x$Freq))})
    #return(temp_df)
    temp_df2 <- do.call(rbind, temp_df2)
    temp_df2$cat <- rownames(temp_df2) 
    colnames(temp_df2) <- c(var2, 'Proporcao', var1)
    rownames(temp_df2) <- NULL
    temp_df2$Proporcao <- round(temp_df2$Proporcao * 100, 1)
    temp_df2[,3] <- substr(temp_df2[,3], 1, nchar(temp_df2[,3])-2)
    temp_df2[,3] <- str_replace(temp_df2[,3], '\\.', '')
    temp_df2[,1] <- factor(as.character(temp_df2[,1]), levels = levels(tbl$Var1))
    
    #return(list(temp_df2, tbl))


    p4 <- ggplot(temp_df2, aes_string(x = var2, y = var1, size = 'Proporcao', col = var2)) +
        geom_point() +
        theme(axis.text.x = element_blank(),
             legend.title = element_blank()) +
        scale_size(range = c(.1, 24)) +
        geom_text(aes(label = paste(temp_df2$Proporcao,'%')), colour = 'black', position = position_nudge(nudgex2, nudgey2), cex = cex, show.legend = FALSE) +
        ylab(labeldict[[var2]]) +
        xlab(labeldict[[var1]]) +
        guides(size=FALSE)
        

        
    
    ret_list <- list(tbl, p1, p2, p3, p4)
    
    return (ret_list)
}
```


```R
plot_relation_multi <- function(var1, var2, reorder = F, levels = F, title = '', ylab = 'Quantidade', xlab = '', 
                                legend_lab = '', width = 7, height = 7, nudgex1 = 0, nudgey1 = 0, nudgex2 = 0, nudgey2 = 0, cex = 2.8){
    
    options(repr.plot.width = width, repr.plot.height = height)
    
    temp_df <- na.exclude(subset(multi_df_long_join, startsWith(multi_df_long$key, c(var1, var2))))
    
    tbl <- data.frame(with(temp_df, table(value, eval(parse(text = var1)))))
    tbl$pct <- round((tbl$Freq / sum(tbl$Freq)) * 100,2)
    tbl <- tbl[!(tbl[,1]==""), ]
    tbl$value <- fct_reorder(tbl$value, tbl$Freq)
    #return (tbl)
    
    if (reorder == T){
        
        tbl$value <- fct_rev(fct_reorder(tbl$value, tbl$Freq))
        }
    
    if (levels == T){
        
        tbl$value <- factor(tbl$value, levels = levels)
    }
    
    

    p1 <- ggplot(tbl, aes(x = Var2, y = Freq, fill = value)) + 
        geom_col() +
        ylab(ylab) + 
        xlab(xlab) +
        labs(fill = legend_lab) +
        ggtitle(title)
    
    p2 <- ggplot(tbl, aes(x = Var2, y = Freq, fill = value)) + 
        geom_col(position = 'fill') +
        ylab('Proporção') + 
        xlab(xlab) +
        labs(fill = legend_lab) +
        ggtitle(title)
    
    #temp_df2 <- data.frame(with(duda, table(eval(parse(text = var1)), eval(parse(text = var2)))))
    temp_df2 <- lapply(split(tbl, tbl$Var2), function(x){data.frame(as.character(x$value),x$Freq / sum(x$Freq))})
    #return(temp_df2)
    temp_df2 <- do.call(rbind,temp_df2)
    temp_df2$cat <- rownames(temp_df2) 
    colnames(temp_df2) <- c(var2, 'Proporcao', var1)
    rownames(temp_df2) <- NULL
    
    temp_df2$Proporcao <- round(temp_df2$Proporcao * 100, 1)
    #return(temp_df2)
    temp_df2[,3] <- substr(temp_df2[,3], 1, nchar(temp_df2[,3])-2)
    temp_df2[,3] <- str_replace(temp_df2[,3], '\\.', '')
    temp_df2[,3] <- factor(as.character(temp_df2[,3]), levels = levels(tbl$Var2))
    #return(temp_df2)




    p3 <- ggplot(temp_df2, aes_string(x = var1, y = var2, size = 'Proporcao', col = var1)) +
        geom_point() +
        theme(axis.text.x = element_blank(),
             legend.title = element_blank()) +
        scale_size(range = c(.1, 24)) +
        xlab(var1)+
        ylab(var2) +
        geom_text(aes(label = paste(temp_df2$Proporcao,'%')), colour = 'black', position = position_nudge(nudgex1, nudgey1), cex = cex, show.legend = FALSE) +
        guides(size=FALSE)
    
    
    
    temp_df3 <- lapply(split(tbl, tbl$value), function(x){data.frame(as.character(x$Var2),x$Freq / sum(x$Freq))})
    #return(temp_df3)
    temp_df3 <- do.call(rbind,temp_df3)
    temp_df3$cat <- rownames(temp_df3) 
    colnames(temp_df3) <- c(var1, 'Proporcao', var2)
    rownames(temp_df3) <- NULL
    
    temp_df3$Proporcao <- round(temp_df3$Proporcao * 100, 1)
    #return(temp_df3)
    temp_df3[,3] <- substr(temp_df3[,3], 1, nchar(temp_df3[,3])-2)
    temp_df3[,3] <- str_replace(temp_df3[,3], '\\.', '')
    temp_df3[,1] <- factor(as.character(temp_df3[,1]), levels = levels(tbl$Var2))
    #return(list(tbl, temp_df3))
    #return(tbl)




    p4 <- ggplot(temp_df3, aes_string(x = var1, y = var2, size = 'Proporcao', col = var1)) +
        geom_point() +
        theme(axis.text.x = element_blank(),
             legend.title = element_blank()) +
        scale_size(range = c(.1, 24)) +
        xlab(var1)+
        ylab(var2) +
        geom_text(aes(label = paste(temp_df3$Proporcao,'%')), colour = 'black', position = position_nudge(nudgex2, nudgey2), cex = cex, show.legend = FALSE) +
        guides(size=FALSE)
    
    
    ret_list <- list(temp_df2, p1, p2, p3, p4)
    return (ret_list)
    
    #return(c(tbl, print(p1), print(p2)))

    
    
}
```

---

# Faixa Etária


### OBS. IMPORTANTE!
#### Categorias <17 e 61< contêm poucas observações, não podemos fazer inferências estatisticamente significantes.


```R
plot_duda('faixa_etaria', reorder = F, title = 'Faixa Etária')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>129</td><td>48.86</td></tr>
	<tr><td>30-45 anos     </td><td> 67</td><td>25.38</td></tr>
	<tr><td>46-60 anos     </td><td> 53</td><td>20.08</td></tr>
	<tr><td>61 anos ou mais</td><td>  6</td><td> 2.27</td></tr>
	<tr><td>até 17 anos    </td><td>  9</td><td> 3.41</td></tr>
</tbody>
</table>




![png](output_18_1.png)


<br><br>

## Faixa Etária X Gênero


```R
ret <- plot_relation('faixa_etaria', 'genero', cex = 3.5)
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 10 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>feminino </td><td>76</td><td>28.79</td></tr>
	<tr><td>30-45 anos     </td><td>feminino </td><td>57</td><td>21.59</td></tr>
	<tr><td>46-60 anos     </td><td>feminino </td><td>31</td><td>11.74</td></tr>
	<tr><td>61 anos ou mais</td><td>feminino </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>feminino </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>18-29 anos     </td><td>masculino</td><td>53</td><td>20.08</td></tr>
	<tr><td>30-45 anos     </td><td>masculino</td><td>10</td><td> 3.79</td></tr>
	<tr><td>46-60 anos     </td><td>masculino</td><td>22</td><td> 8.33</td></tr>
	<tr><td>61 anos ou mais</td><td>masculino</td><td> 4</td><td> 1.52</td></tr>
	<tr><td>até 17 anos    </td><td>masculino</td><td> 1</td><td> 0.38</td></tr>
</tbody>
</table>




![png](output_20_1.png)



![png](output_20_2.png)



![png](output_20_3.png)



![png](output_20_4.png)



<br><br>
# Faixa Etária X Dificuldade de compra
#### Maior dificudade é a falta de tempo e a preguiça, porém jovens têm menos tempo. 30-45 anos têm a menor dificuldade de compra online!


```R
ret <- plot_relation('faixa_etaria', 'dificuldade_compra', width = 9, title = 'Dificuldade de Compra Online')
ret[1]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[5])
```


<ol>
	<li><table>
<caption>A data.frame: 40 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>distância grande das lojas que gosto     </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>30-45 anos     </td><td>distância grande das lojas que gosto     </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>46-60 anos     </td><td>distância grande das lojas que gosto     </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>distância grande das lojas que gosto     </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>distância grande das lojas que gosto     </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>falta de estacionamento nos locais       </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>30-45 anos     </td><td>falta de estacionamento nos locais       </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>46-60 anos     </td><td>falta de estacionamento nos locais       </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>61 anos ou mais</td><td>falta de estacionamento nos locais       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>falta de estacionamento nos locais       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>falta de tempo                           </td><td>50</td><td>18.94</td></tr>
	<tr><td>30-45 anos     </td><td>falta de tempo                           </td><td>23</td><td> 8.71</td></tr>
	<tr><td>46-60 anos     </td><td>falta de tempo                           </td><td>13</td><td> 4.92</td></tr>
	<tr><td>61 anos ou mais</td><td>falta de tempo                           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>falta de tempo                           </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>18-29 anos     </td><td>não gosto de ir ao supermercado          </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>30-45 anos     </td><td>não gosto de ir ao supermercado          </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>não gosto de ir ao supermercado          </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>não gosto de ir ao supermercado          </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não gosto de ir ao supermercado          </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não gosto de ir em shoppings             </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>30-45 anos     </td><td>não gosto de ir em shoppings             </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>46-60 anos     </td><td>não gosto de ir em shoppings             </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>não gosto de ir em shoppings             </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>não gosto de ir em shoppings             </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não gosto de ter contato com vendedores  </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>30-45 anos     </td><td>não gosto de ter contato com vendedores  </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>46-60 anos     </td><td>não gosto de ter contato com vendedores  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>não gosto de ter contato com vendedores  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não gosto de ter contato com vendedores  </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>não tenho dificuldades para fazer compras</td><td>26</td><td> 9.85</td></tr>
	<tr><td>30-45 anos     </td><td>não tenho dificuldades para fazer compras</td><td> 9</td><td> 3.41</td></tr>
	<tr><td>46-60 anos     </td><td>não tenho dificuldades para fazer compras</td><td>21</td><td> 7.95</td></tr>
	<tr><td>61 anos ou mais</td><td>não tenho dificuldades para fazer compras</td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>não tenho dificuldades para fazer compras</td><td> 2</td><td> 0.76</td></tr>
	<tr><td>18-29 anos     </td><td>tenho preguiça de ir até as lojas        </td><td>28</td><td>10.61</td></tr>
	<tr><td>30-45 anos     </td><td>tenho preguiça de ir até as lojas        </td><td>15</td><td> 5.68</td></tr>
	<tr><td>46-60 anos     </td><td>tenho preguiça de ir até as lojas        </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>61 anos ou mais</td><td>tenho preguiça de ir até as lojas        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>tenho preguiça de ir até as lojas        </td><td> 2</td><td> 0.76</td></tr>
</tbody>
</table>
</li>
</ol>




![png](output_22_1.png)



![png](output_22_2.png)


    [[1]]



![png](output_22_4.png)


    



![png](output_22_6.png)



<br><br>
# Faixa Etária X Tipo de Loja
#### Menores de 17 anos preferem sapatos, porém amostra é pequena para ser estatísticamente significante. Podemos investigar melhor, mas não podemos afirmar com confiança.<br>
#### Jovens compram mais roupas. Acima de 30 anos vão mais ao supermercado. 30-45 compram mais brinquedos (filhos novos)


```R
ret <- plot_relation_multi('faixa_etaria', 'tipo_loja', title = 'Tipo de Loja')
ret[1]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<ol>
	<li><table>
<caption>A data.frame: 50 × 3</caption>
<thead>
	<tr><th scope=col>tipo_loja</th><th scope=col>Proporcao</th><th scope=col>faixa_etaria</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>brinquedos          </td><td> 0.0</td><td>18-29 anos     </td></tr>
	<tr><td>eletrodomêsticos    </td><td> 0.0</td><td>18-29 anos     </td></tr>
	<tr><td>esportes            </td><td>11.0</td><td>18-29 anos     </td></tr>
	<tr><td>livrarias           </td><td>11.0</td><td>18-29 anos     </td></tr>
	<tr><td>móveis/ decoração   </td><td> 3.9</td><td>18-29 anos     </td></tr>
	<tr><td>outros              </td><td> 3.3</td><td>18-29 anos     </td></tr>
	<tr><td>produtos eletrônicos</td><td> 8.3</td><td>18-29 anos     </td></tr>
	<tr><td>roupas              </td><td>34.3</td><td>18-29 anos     </td></tr>
	<tr><td>sapatos             </td><td>13.8</td><td>18-29 anos     </td></tr>
	<tr><td>supermercado        </td><td>14.4</td><td>18-29 anos     </td></tr>
	<tr><td>brinquedos          </td><td> 7.6</td><td>30-45 anos     </td></tr>
	<tr><td>eletrodomêsticos    </td><td> 0.0</td><td>30-45 anos     </td></tr>
	<tr><td>esportes            </td><td> 6.7</td><td>30-45 anos     </td></tr>
	<tr><td>livrarias           </td><td>16.2</td><td>30-45 anos     </td></tr>
	<tr><td>móveis/ decoração   </td><td> 5.7</td><td>30-45 anos     </td></tr>
	<tr><td>outros              </td><td> 3.8</td><td>30-45 anos     </td></tr>
	<tr><td>produtos eletrônicos</td><td> 2.9</td><td>30-45 anos     </td></tr>
	<tr><td>roupas              </td><td>22.9</td><td>30-45 anos     </td></tr>
	<tr><td>sapatos             </td><td>19.0</td><td>30-45 anos     </td></tr>
	<tr><td>supermercado        </td><td>15.2</td><td>30-45 anos     </td></tr>
	<tr><td>brinquedos          </td><td> 1.4</td><td>46-60 anos     </td></tr>
	<tr><td>eletrodomêsticos    </td><td> 4.2</td><td>46-60 anos     </td></tr>
	<tr><td>esportes            </td><td> 8.5</td><td>46-60 anos     </td></tr>
	<tr><td>livrarias           </td><td> 7.0</td><td>46-60 anos     </td></tr>
	<tr><td>móveis/ decoração   </td><td> 4.2</td><td>46-60 anos     </td></tr>
	<tr><td>outros              </td><td> 2.8</td><td>46-60 anos     </td></tr>
	<tr><td>produtos eletrônicos</td><td> 1.4</td><td>46-60 anos     </td></tr>
	<tr><td>roupas              </td><td>31.0</td><td>46-60 anos     </td></tr>
	<tr><td>sapatos             </td><td>15.5</td><td>46-60 anos     </td></tr>
	<tr><td>supermercado        </td><td>23.9</td><td>46-60 anos     </td></tr>
	<tr><td>brinquedos          </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>eletrodomêsticos    </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>esportes            </td><td>11.1</td><td>61 anos ou mais</td></tr>
	<tr><td>livrarias           </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>móveis/ decoração   </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>outros              </td><td>11.1</td><td>61 anos ou mais</td></tr>
	<tr><td>produtos eletrônicos</td><td>22.2</td><td>61 anos ou mais</td></tr>
	<tr><td>roupas              </td><td>22.2</td><td>61 anos ou mais</td></tr>
	<tr><td>sapatos             </td><td>11.1</td><td>61 anos ou mais</td></tr>
	<tr><td>supermercado        </td><td>22.2</td><td>61 anos ou mais</td></tr>
	<tr><td>brinquedos          </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>eletrodomêsticos    </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>esportes            </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>livrarias           </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>móveis/ decoração   </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>outros              </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>produtos eletrônicos</td><td>11.1</td><td>até 17 anos    </td></tr>
	<tr><td>roupas              </td><td>44.4</td><td>até 17 anos    </td></tr>
	<tr><td>sapatos             </td><td>44.4</td><td>até 17 anos    </td></tr>
	<tr><td>supermercado        </td><td> 0.0</td><td>até 17 anos    </td></tr>
</tbody>
</table>
</li>
</ol>




![png](output_24_1.png)



![png](output_24_2.png)



![png](output_24_3.png)



![png](output_24_4.png)



<br><br>
# Faixa Etária X Fonte de informação



```R
ret <- plot_relation_multi('faixa_etaria', 'fonte_informacao', title = 'Fonte de Informação', width = 9)
ret[[1]]
print(ret[[2]])
print(ret[3])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 60 × 3</caption>
<thead>
	<tr><th scope=col>fonte_informacao</th><th scope=col>Proporcao</th><th scope=col>faixa_etaria</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>amigos/ colegas                    </td><td>12.6</td><td>18-29 anos     </td></tr>
	<tr><td>blogueiras/os                      </td><td> 9.8</td><td>18-29 anos     </td></tr>
	<tr><td>email marketing                    </td><td> 2.3</td><td>18-29 anos     </td></tr>
	<tr><td>outros                             </td><td> 0.5</td><td>18-29 anos     </td></tr>
	<tr><td>pesquisando na internet            </td><td>21.9</td><td>18-29 anos     </td></tr>
	<tr><td>propagandas em revista/jornal      </td><td> 1.9</td><td>18-29 anos     </td></tr>
	<tr><td>propagandas redes sociais          </td><td>18.1</td><td>18-29 anos     </td></tr>
	<tr><td>seguindo as lojas nas redes sociais</td><td>14.0</td><td>18-29 anos     </td></tr>
	<tr><td>sites de promoção                  </td><td> 6.0</td><td>18-29 anos     </td></tr>
	<tr><td>televisão                          </td><td> 0.0</td><td>18-29 anos     </td></tr>
	<tr><td>vitrines                           </td><td> 9.3</td><td>18-29 anos     </td></tr>
	<tr><td>WhatsApp                           </td><td> 3.7</td><td>18-29 anos     </td></tr>
	<tr><td>amigos/ colegas                    </td><td> 4.1</td><td>30-45 anos     </td></tr>
	<tr><td>blogueiras/os                      </td><td> 7.2</td><td>30-45 anos     </td></tr>
	<tr><td>email marketing                    </td><td> 0.0</td><td>30-45 anos     </td></tr>
	<tr><td>outros                             </td><td> 0.0</td><td>30-45 anos     </td></tr>
	<tr><td>pesquisando na internet            </td><td>12.4</td><td>30-45 anos     </td></tr>
	<tr><td>propagandas em revista/jornal      </td><td> 3.1</td><td>30-45 anos     </td></tr>
	<tr><td>propagandas redes sociais          </td><td>17.5</td><td>30-45 anos     </td></tr>
	<tr><td>seguindo as lojas nas redes sociais</td><td>21.6</td><td>30-45 anos     </td></tr>
	<tr><td>sites de promoção                  </td><td> 8.2</td><td>30-45 anos     </td></tr>
	<tr><td>televisão                          </td><td> 1.0</td><td>30-45 anos     </td></tr>
	<tr><td>vitrines                           </td><td>17.5</td><td>30-45 anos     </td></tr>
	<tr><td>WhatsApp                           </td><td> 7.2</td><td>30-45 anos     </td></tr>
	<tr><td>amigos/ colegas                    </td><td> 5.3</td><td>46-60 anos     </td></tr>
	<tr><td>blogueiras/os                      </td><td> 9.3</td><td>46-60 anos     </td></tr>
	<tr><td>email marketing                    </td><td> 1.3</td><td>46-60 anos     </td></tr>
	<tr><td>outros                             </td><td> 0.0</td><td>46-60 anos     </td></tr>
	<tr><td>pesquisando na internet            </td><td>13.3</td><td>46-60 anos     </td></tr>
	<tr><td>propagandas em revista/jornal      </td><td> 5.3</td><td>46-60 anos     </td></tr>
	<tr><td>propagandas redes sociais          </td><td>16.0</td><td>46-60 anos     </td></tr>
	<tr><td>seguindo as lojas nas redes sociais</td><td>18.7</td><td>46-60 anos     </td></tr>
	<tr><td>sites de promoção                  </td><td> 8.0</td><td>46-60 anos     </td></tr>
	<tr><td>televisão                          </td><td> 4.0</td><td>46-60 anos     </td></tr>
	<tr><td>vitrines                           </td><td>10.7</td><td>46-60 anos     </td></tr>
	<tr><td>WhatsApp                           </td><td> 8.0</td><td>46-60 anos     </td></tr>
	<tr><td>amigos/ colegas                    </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>blogueiras/os                      </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>email marketing                    </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>outros                             </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>pesquisando na internet            </td><td>33.3</td><td>61 anos ou mais</td></tr>
	<tr><td>propagandas em revista/jornal      </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>propagandas redes sociais          </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>seguindo as lojas nas redes sociais</td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>sites de promoção                  </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>televisão                          </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>vitrines                           </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>WhatsApp                           </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>amigos/ colegas                    </td><td> 5.6</td><td>até 17 anos    </td></tr>
	<tr><td>blogueiras/os                      </td><td>22.2</td><td>até 17 anos    </td></tr>
	<tr><td>email marketing                    </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>outros                             </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>pesquisando na internet            </td><td>11.1</td><td>até 17 anos    </td></tr>
	<tr><td>propagandas em revista/jornal      </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>propagandas redes sociais          </td><td>16.7</td><td>até 17 anos    </td></tr>
	<tr><td>seguindo as lojas nas redes sociais</td><td>27.8</td><td>até 17 anos    </td></tr>
	<tr><td>sites de promoção                  </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>televisão                          </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>vitrines                           </td><td>11.1</td><td>até 17 anos    </td></tr>
	<tr><td>WhatsApp                           </td><td> 5.6</td><td>até 17 anos    </td></tr>
</tbody>
</table>



    [[1]]



![png](output_26_2.png)


    



![png](output_26_4.png)



![png](output_26_5.png)



![png](output_26_6.png)



<br><br>
# Faixa Etária X O que mais leva em consideração na hora de comprar online



```R
ret <- plot_relation_multi('faixa_etaria', 'consideracao_loja', width = 10, title = 'O que mais leva em consideração na hora de fazer uma compra online?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 60 × 3</caption>
<thead>
	<tr><th scope=col>consideracao_loja</th><th scope=col>Proporcao</th><th scope=col>faixa_etaria</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>confiança na marca/produto                    </td><td>16.0</td><td>18-29 anos     </td></tr>
	<tr><td>diferentes opções de entrega do produto       </td><td> 1.5</td><td>18-29 anos     </td></tr>
	<tr><td>experiencia de compra na loja                 </td><td> 7.8</td><td>18-29 anos     </td></tr>
	<tr><td>informações sobre o produto                   </td><td> 9.3</td><td>18-29 anos     </td></tr>
	<tr><td>localização da loja                           </td><td> 4.9</td><td>18-29 anos     </td></tr>
	<tr><td>padronização do produto em relação a numeração</td><td> 1.9</td><td>18-29 anos     </td></tr>
	<tr><td>presença nas mídias sociais                   </td><td> 3.4</td><td>18-29 anos     </td></tr>
	<tr><td>produto personalizado/ exclusivo              </td><td> 6.3</td><td>18-29 anos     </td></tr>
	<tr><td>promoção                                      </td><td>12.3</td><td>18-29 anos     </td></tr>
	<tr><td>qualidade do atendimento                      </td><td> 8.6</td><td>18-29 anos     </td></tr>
	<tr><td>qualidade do produto                          </td><td>21.6</td><td>18-29 anos     </td></tr>
	<tr><td>venda online                                  </td><td> 6.3</td><td>18-29 anos     </td></tr>
	<tr><td>confiança na marca/produto                    </td><td>10.0</td><td>30-45 anos     </td></tr>
	<tr><td>diferentes opções de entrega do produto       </td><td> 0.8</td><td>30-45 anos     </td></tr>
	<tr><td>experiencia de compra na loja                 </td><td> 8.5</td><td>30-45 anos     </td></tr>
	<tr><td>informações sobre o produto                   </td><td> 4.6</td><td>30-45 anos     </td></tr>
	<tr><td>localização da loja                           </td><td>11.5</td><td>30-45 anos     </td></tr>
	<tr><td>padronização do produto em relação a numeração</td><td> 3.8</td><td>30-45 anos     </td></tr>
	<tr><td>presença nas mídias sociais                   </td><td> 2.3</td><td>30-45 anos     </td></tr>
	<tr><td>produto personalizado/ exclusivo              </td><td> 2.3</td><td>30-45 anos     </td></tr>
	<tr><td>promoção                                      </td><td>14.6</td><td>30-45 anos     </td></tr>
	<tr><td>qualidade do atendimento                      </td><td>14.6</td><td>30-45 anos     </td></tr>
	<tr><td>qualidade do produto                          </td><td>21.5</td><td>30-45 anos     </td></tr>
	<tr><td>venda online                                  </td><td> 5.4</td><td>30-45 anos     </td></tr>
	<tr><td>confiança na marca/produto                    </td><td>14.7</td><td>46-60 anos     </td></tr>
	<tr><td>diferentes opções de entrega do produto       </td><td> 0.0</td><td>46-60 anos     </td></tr>
	<tr><td>experiencia de compra na loja                 </td><td>10.5</td><td>46-60 anos     </td></tr>
	<tr><td>informações sobre o produto                   </td><td> 8.4</td><td>46-60 anos     </td></tr>
	<tr><td>localização da loja                           </td><td>14.7</td><td>46-60 anos     </td></tr>
	<tr><td>padronização do produto em relação a numeração</td><td> 2.1</td><td>46-60 anos     </td></tr>
	<tr><td>presença nas mídias sociais                   </td><td> 3.2</td><td>46-60 anos     </td></tr>
	<tr><td>produto personalizado/ exclusivo              </td><td> 4.2</td><td>46-60 anos     </td></tr>
	<tr><td>promoção                                      </td><td> 9.5</td><td>46-60 anos     </td></tr>
	<tr><td>qualidade do atendimento                      </td><td> 8.4</td><td>46-60 anos     </td></tr>
	<tr><td>qualidade do produto                          </td><td>18.9</td><td>46-60 anos     </td></tr>
	<tr><td>venda online                                  </td><td> 5.3</td><td>46-60 anos     </td></tr>
	<tr><td>confiança na marca/produto                    </td><td> 8.3</td><td>61 anos ou mais</td></tr>
	<tr><td>diferentes opções de entrega do produto       </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>experiencia de compra na loja                 </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>informações sobre o produto                   </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>localização da loja                           </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>padronização do produto em relação a numeração</td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>presença nas mídias sociais                   </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>produto personalizado/ exclusivo              </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>promoção                                      </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>qualidade do atendimento                      </td><td>16.7</td><td>61 anos ou mais</td></tr>
	<tr><td>qualidade do produto                          </td><td> 8.3</td><td>61 anos ou mais</td></tr>
	<tr><td>venda online                                  </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>confiança na marca/produto                    </td><td>12.5</td><td>até 17 anos    </td></tr>
	<tr><td>diferentes opções de entrega do produto       </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>experiencia de compra na loja                 </td><td>12.5</td><td>até 17 anos    </td></tr>
	<tr><td>informações sobre o produto                   </td><td> 6.2</td><td>até 17 anos    </td></tr>
	<tr><td>localização da loja                           </td><td>12.5</td><td>até 17 anos    </td></tr>
	<tr><td>padronização do produto em relação a numeração</td><td> 6.2</td><td>até 17 anos    </td></tr>
	<tr><td>presença nas mídias sociais                   </td><td> 6.2</td><td>até 17 anos    </td></tr>
	<tr><td>produto personalizado/ exclusivo              </td><td> 6.2</td><td>até 17 anos    </td></tr>
	<tr><td>promoção                                      </td><td> 6.2</td><td>até 17 anos    </td></tr>
	<tr><td>qualidade do atendimento                      </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>qualidade do produto                          </td><td>31.2</td><td>até 17 anos    </td></tr>
	<tr><td>venda online                                  </td><td> 0.0</td><td>até 17 anos    </td></tr>
</tbody>
</table>




![png](output_28_1.png)



![png](output_28_2.png)



![png](output_28_3.png)



![png](output_28_4.png)



<br><br>
# Faixa Etária X Relação com loja online
### Existe uma relacão negativa entre idade e compra online (+ idade - compra online)


```R
ret <- plot_relation_multi('faixa_etaria', 'relacao_loja_online', title = 'Como é sua relacao com as lojas online?', width = 11)
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 35 × 3</caption>
<thead>
	<tr><th scope=col>relacao_loja_online</th><th scope=col>Proporcao</th><th scope=col>faixa_etaria</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>abro os emails marketing que recebo das lojas que gosto                  </td><td> 7.8</td><td>18-29 anos     </td></tr>
	<tr><td>estou sempre visitando o site das lojas que gosto                        </td><td>22.5</td><td>18-29 anos     </td></tr>
	<tr><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais</td><td>10.9</td><td>18-29 anos     </td></tr>
	<tr><td>sempre entro em contato com o vendedor da loja pelo WhatsApp             </td><td> 3.9</td><td>18-29 anos     </td></tr>
	<tr><td>sigo as lojas que gosto nas redes sociais                                </td><td>32.6</td><td>18-29 anos     </td></tr>
	<tr><td>tenho o aplicativo das lojas que gosto                                   </td><td> 7.8</td><td>18-29 anos     </td></tr>
	<tr><td>vejo propaganda nas redes sociais                                        </td><td>14.7</td><td>18-29 anos     </td></tr>
	<tr><td>abro os emails marketing que recebo das lojas que gosto                  </td><td> 7.9</td><td>30-45 anos     </td></tr>
	<tr><td>estou sempre visitando o site das lojas que gosto                        </td><td>30.3</td><td>30-45 anos     </td></tr>
	<tr><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais</td><td> 1.3</td><td>30-45 anos     </td></tr>
	<tr><td>sempre entro em contato com o vendedor da loja pelo WhatsApp             </td><td> 7.9</td><td>30-45 anos     </td></tr>
	<tr><td>sigo as lojas que gosto nas redes sociais                                </td><td>34.2</td><td>30-45 anos     </td></tr>
	<tr><td>tenho o aplicativo das lojas que gosto                                   </td><td> 7.9</td><td>30-45 anos     </td></tr>
	<tr><td>vejo propaganda nas redes sociais                                        </td><td>10.5</td><td>30-45 anos     </td></tr>
	<tr><td>abro os emails marketing que recebo das lojas que gosto                  </td><td> 5.9</td><td>46-60 anos     </td></tr>
	<tr><td>estou sempre visitando o site das lojas que gosto                        </td><td>29.4</td><td>46-60 anos     </td></tr>
	<tr><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais</td><td> 3.9</td><td>46-60 anos     </td></tr>
	<tr><td>sempre entro em contato com o vendedor da loja pelo WhatsApp             </td><td> 5.9</td><td>46-60 anos     </td></tr>
	<tr><td>sigo as lojas que gosto nas redes sociais                                </td><td>27.5</td><td>46-60 anos     </td></tr>
	<tr><td>tenho o aplicativo das lojas que gosto                                   </td><td> 9.8</td><td>46-60 anos     </td></tr>
	<tr><td>vejo propaganda nas redes sociais                                        </td><td>17.6</td><td>46-60 anos     </td></tr>
	<tr><td>abro os emails marketing que recebo das lojas que gosto                  </td><td>33.3</td><td>61 anos ou mais</td></tr>
	<tr><td>estou sempre visitando o site das lojas que gosto                        </td><td>33.3</td><td>61 anos ou mais</td></tr>
	<tr><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais</td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>sempre entro em contato com o vendedor da loja pelo WhatsApp             </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>sigo as lojas que gosto nas redes sociais                                </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>tenho o aplicativo das lojas que gosto                                   </td><td>33.3</td><td>61 anos ou mais</td></tr>
	<tr><td>vejo propaganda nas redes sociais                                        </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>abro os emails marketing que recebo das lojas que gosto                  </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>estou sempre visitando o site das lojas que gosto                        </td><td>14.3</td><td>até 17 anos    </td></tr>
	<tr><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais</td><td>14.3</td><td>até 17 anos    </td></tr>
	<tr><td>sempre entro em contato com o vendedor da loja pelo WhatsApp             </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>sigo as lojas que gosto nas redes sociais                                </td><td>57.1</td><td>até 17 anos    </td></tr>
	<tr><td>tenho o aplicativo das lojas que gosto                                   </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>vejo propaganda nas redes sociais                                        </td><td>14.3</td><td>até 17 anos    </td></tr>
</tbody>
</table>




![png](output_30_1.png)



![png](output_30_2.png)



![png](output_30_3.png)



![png](output_30_4.png)



<br><br>
# Faixa Etária X Meio de compra online favorito
### Existe uma relacão negativa entre idade e compra online (+ idade - compra online)


```R
ret <- plot_relation('faixa_etaria', 'meio_preferido', title = 'Qual seu meio de compra online preferido?', width = 9)
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 35 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>loja física                                  </td><td>93</td><td>35.23</td></tr>
	<tr><td>30-45 anos     </td><td>loja física                                  </td><td>39</td><td>14.77</td></tr>
	<tr><td>46-60 anos     </td><td>loja física                                  </td><td>37</td><td>14.02</td></tr>
	<tr><td>61 anos ou mais</td><td>loja física                                  </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>até 17 anos    </td><td>loja física                                  </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>18-29 anos     </td><td>outro                                        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>30-45 anos     </td><td>outro                                        </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>46-60 anos     </td><td>outro                                        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>outro                                        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>outro                                        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por aplicativo                               </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>30-45 anos     </td><td>por aplicativo                               </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>46-60 anos     </td><td>por aplicativo                               </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>61 anos ou mais</td><td>por aplicativo                               </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>por aplicativo                               </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>por catálogo                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>30-45 anos     </td><td>por catálogo                                 </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>por catálogo                                 </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>por catálogo                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por catálogo                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por telefone                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>30-45 anos     </td><td>por telefone                                 </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>por telefone                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>por telefone                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por telefone                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>redes sociais (Instagram, facebook, WhatsApp)</td><td> 5</td><td> 1.89</td></tr>
	<tr><td>30-45 anos     </td><td>redes sociais (Instagram, facebook, WhatsApp)</td><td> 9</td><td> 3.41</td></tr>
	<tr><td>46-60 anos     </td><td>redes sociais (Instagram, facebook, WhatsApp)</td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>redes sociais (Instagram, facebook, WhatsApp)</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>redes sociais (Instagram, facebook, WhatsApp)</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>site da loja                                 </td><td>29</td><td>10.98</td></tr>
	<tr><td>30-45 anos     </td><td>site da loja                                 </td><td>12</td><td> 4.55</td></tr>
	<tr><td>46-60 anos     </td><td>site da loja                                 </td><td> 9</td><td> 3.41</td></tr>
	<tr><td>61 anos ou mais</td><td>site da loja                                 </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>site da loja                                 </td><td> 0</td><td> 0.00</td></tr>
</tbody>
</table>




![png](output_32_1.png)



![png](output_32_2.png)



![png](output_32_3.png)



![png](output_32_4.png)



<br><br>
# Faixa Etária X Papel da internet



```R
ret <- plot_relation('faixa_etaria', 'papel_internet', width = 11, title = 'Papel da Internet')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 40 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>é na internet que vejo propagandas e promoções que me levam a compra</td><td>21</td><td> 7.95</td></tr>
	<tr><td>30-45 anos     </td><td>é na internet que vejo propagandas e promoções que me levam a compra</td><td>11</td><td> 4.17</td></tr>
	<tr><td>46-60 anos     </td><td>é na internet que vejo propagandas e promoções que me levam a compra</td><td> 9</td><td> 3.41</td></tr>
	<tr><td>61 anos ou mais</td><td>é na internet que vejo propagandas e promoções que me levam a compra</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>é na internet que vejo propagandas e promoções que me levam a compra</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>faço pesquisa de preço  na internet                                 </td><td>19</td><td> 7.20</td></tr>
	<tr><td>30-45 anos     </td><td>faço pesquisa de preço  na internet                                 </td><td>13</td><td> 4.92</td></tr>
	<tr><td>46-60 anos     </td><td>faço pesquisa de preço  na internet                                 </td><td>15</td><td> 5.68</td></tr>
	<tr><td>61 anos ou mais</td><td>faço pesquisa de preço  na internet                                 </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>faço pesquisa de preço  na internet                                 </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>18-29 anos     </td><td>não uso a internet antes de fazer uma compra                        </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>30-45 anos     </td><td>não uso a internet antes de fazer uma compra                        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>não uso a internet antes de fazer uma compra                        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>não uso a internet antes de fazer uma compra                        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não uso a internet antes de fazer uma compra                        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>pesquiso informações sobre produtos na internet                     </td><td>26</td><td> 9.85</td></tr>
	<tr><td>30-45 anos     </td><td>pesquiso informações sobre produtos na internet                     </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>46-60 anos     </td><td>pesquiso informações sobre produtos na internet                     </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>61 anos ou mais</td><td>pesquiso informações sobre produtos na internet                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>pesquiso informações sobre produtos na internet                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>saber as novidades das lojas                                        </td><td>36</td><td>13.64</td></tr>
	<tr><td>30-45 anos     </td><td>saber as novidades das lojas                                        </td><td>24</td><td> 9.09</td></tr>
	<tr><td>46-60 anos     </td><td>saber as novidades das lojas                                        </td><td>16</td><td> 6.06</td></tr>
	<tr><td>61 anos ou mais</td><td>saber as novidades das lojas                                        </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>até 17 anos    </td><td>saber as novidades das lojas                                        </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>18-29 anos     </td><td>uso a internet somente para realizar a compra                       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>uso a internet somente para realizar a compra                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>uso a internet somente para realizar a compra                       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>uso a internet somente para realizar a compra                       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>uso a internet somente para realizar a compra                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>utilizo a internet em todo meu processo de compra                   </td><td>17</td><td> 6.44</td></tr>
	<tr><td>30-45 anos     </td><td>utilizo a internet em todo meu processo de compra                   </td><td>12</td><td> 4.55</td></tr>
	<tr><td>46-60 anos     </td><td>utilizo a internet em todo meu processo de compra                   </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>utilizo a internet em todo meu processo de compra                   </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>utilizo a internet em todo meu processo de compra                   </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>vejo avaliações de lojas na internet                                </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>30-45 anos     </td><td>vejo avaliações de lojas na internet                                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>vejo avaliações de lojas na internet                                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>vejo avaliações de lojas na internet                                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>vejo avaliações de lojas na internet                                </td><td> 0</td><td> 0.00</td></tr>
</tbody>
</table>




![png](output_34_1.png)



![png](output_34_2.png)



![png](output_34_3.png)



![png](output_34_4.png)


## Faixa Etária X Faz compra online 


```R
ret <- plot_relation('faixa_etaria', 'faz_compra_online', width = 9, title = 'Dificuldade de Compra Online')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 15 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>não, não gosto de comprar online             </td><td>18</td><td> 6.82</td></tr>
	<tr><td>30-45 anos     </td><td>não, não gosto de comprar online             </td><td>10</td><td> 3.79</td></tr>
	<tr><td>46-60 anos     </td><td>não, não gosto de comprar online             </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>61 anos ou mais</td><td>não, não gosto de comprar online             </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não, não gosto de comprar online             </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>18-29 anos     </td><td>sim, faço a maioria das minhas compras online</td><td>23</td><td> 8.71</td></tr>
	<tr><td>30-45 anos     </td><td>sim, faço a maioria das minhas compras online</td><td>16</td><td> 6.06</td></tr>
	<tr><td>46-60 anos     </td><td>sim, faço a maioria das minhas compras online</td><td> 7</td><td> 2.65</td></tr>
	<tr><td>61 anos ou mais</td><td>sim, faço a maioria das minhas compras online</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>sim, faço a maioria das minhas compras online</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>sim, faço algumas compras online             </td><td>88</td><td>33.33</td></tr>
	<tr><td>30-45 anos     </td><td>sim, faço algumas compras online             </td><td>41</td><td>15.53</td></tr>
	<tr><td>46-60 anos     </td><td>sim, faço algumas compras online             </td><td>38</td><td>14.39</td></tr>
	<tr><td>61 anos ou mais</td><td>sim, faço algumas compras online             </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>até 17 anos    </td><td>sim, faço algumas compras online             </td><td> 7</td><td> 2.65</td></tr>
</tbody>
</table>




![png](output_36_1.png)



![png](output_36_2.png)



![png](output_36_3.png)



![png](output_36_4.png)


## Faixa Etária X Tipo de Produto


```R
ret <- plot_relation_multi('faixa_etaria', 'tipos_prod_online', width = 10, title = 'Tipos de Produto')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 75 × 3</caption>
<thead>
	<tr><th scope=col>tipos_prod_online</th><th scope=col>Proporcao</th><th scope=col>faixa_etaria</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>acessórios                        </td><td> 9.6</td><td>18-29 anos</td></tr>
	<tr><td>eletrodomêsticos                  </td><td> 1.9</td><td>18-29 anos</td></tr>
	<tr><td>eletrónicos                       </td><td> 7.7</td><td>18-29 anos</td></tr>
	<tr><td>fármacia                          </td><td> 2.7</td><td>18-29 anos</td></tr>
	<tr><td>filmes                            </td><td> 3.5</td><td>18-29 anos</td></tr>
	<tr><td>ingressos de cinema/ shows/ teatro</td><td>16.2</td><td>18-29 anos</td></tr>
	<tr><td>livros                            </td><td> 4.2</td><td>18-29 anos</td></tr>
	<tr><td>não compro online                 </td><td> 1.5</td><td>18-29 anos</td></tr>
	<tr><td>outros                            </td><td> 0.0</td><td>18-29 anos</td></tr>
	<tr><td>passagem aérea                    </td><td>16.9</td><td>18-29 anos</td></tr>
	<tr><td>produtos de beleza                </td><td> 4.6</td><td>18-29 anos</td></tr>
	<tr><td>produtos esportivos               </td><td> 5.0</td><td>18-29 anos</td></tr>
	<tr><td>roupas                            </td><td>15.0</td><td>18-29 anos</td></tr>
	<tr><td>sapato                            </td><td>11.2</td><td>18-29 anos</td></tr>
	<tr><td>supermercado                      </td><td> 0.0</td><td>18-29 anos</td></tr>
	<tr><td>acessórios                        </td><td> 7.1</td><td>30-45 anos</td></tr>
	<tr><td>eletrodomêsticos                  </td><td> 3.6</td><td>30-45 anos</td></tr>
	<tr><td>eletrónicos                       </td><td> 6.5</td><td>30-45 anos</td></tr>
	<tr><td>fármacia                          </td><td> 5.3</td><td>30-45 anos</td></tr>
	<tr><td>filmes                            </td><td> 1.8</td><td>30-45 anos</td></tr>
	<tr><td>ingressos de cinema/ shows/ teatro</td><td> 9.5</td><td>30-45 anos</td></tr>
	<tr><td>livros                            </td><td> 9.5</td><td>30-45 anos</td></tr>
	<tr><td>não compro online                 </td><td> 0.6</td><td>30-45 anos</td></tr>
	<tr><td>outros                            </td><td> 0.0</td><td>30-45 anos</td></tr>
	<tr><td>passagem aérea                    </td><td>14.2</td><td>30-45 anos</td></tr>
	<tr><td>produtos de beleza                </td><td>10.1</td><td>30-45 anos</td></tr>
	<tr><td>produtos esportivos               </td><td> 4.7</td><td>30-45 anos</td></tr>
	<tr><td>roupas                            </td><td>16.0</td><td>30-45 anos</td></tr>
	<tr><td>sapato                            </td><td>10.7</td><td>30-45 anos</td></tr>
	<tr><td>supermercado                      </td><td> 0.6</td><td>30-45 anos</td></tr>
	<tr><td>⋮</td><td>⋮</td><td>⋮</td></tr>
	<tr><td>acessórios                        </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>eletrodomêsticos                  </td><td>28.6</td><td>61 anos ou mais</td></tr>
	<tr><td>eletrónicos                       </td><td>14.3</td><td>61 anos ou mais</td></tr>
	<tr><td>fármacia                          </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>filmes                            </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>ingressos de cinema/ shows/ teatro</td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>livros                            </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>não compro online                 </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>outros                            </td><td>14.3</td><td>61 anos ou mais</td></tr>
	<tr><td>passagem aérea                    </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>produtos de beleza                </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>produtos esportivos               </td><td>14.3</td><td>61 anos ou mais</td></tr>
	<tr><td>roupas                            </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>sapato                            </td><td>28.6</td><td>61 anos ou mais</td></tr>
	<tr><td>supermercado                      </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>acessórios                        </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>eletrodomêsticos                  </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>eletrónicos                       </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>fármacia                          </td><td> 5.9</td><td>até 17 anos    </td></tr>
	<tr><td>filmes                            </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>ingressos de cinema/ shows/ teatro</td><td> 5.9</td><td>até 17 anos    </td></tr>
	<tr><td>livros                            </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>não compro online                 </td><td> 5.9</td><td>até 17 anos    </td></tr>
	<tr><td>outros                            </td><td> 5.9</td><td>até 17 anos    </td></tr>
	<tr><td>passagem aérea                    </td><td>35.3</td><td>até 17 anos    </td></tr>
	<tr><td>produtos de beleza                </td><td>11.8</td><td>até 17 anos    </td></tr>
	<tr><td>produtos esportivos               </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>roupas                            </td><td>17.6</td><td>até 17 anos    </td></tr>
	<tr><td>sapato                            </td><td>11.8</td><td>até 17 anos    </td></tr>
	<tr><td>supermercado                      </td><td> 0.0</td><td>até 17 anos    </td></tr>
</tbody>
</table>




![png](output_38_1.png)



![png](output_38_2.png)



![png](output_38_3.png)



![png](output_38_4.png)


## Faixa Etária X Mesma loja Diferentes maneiras


```R
ret <- plot_relation('faixa_etaria', 'mesma_loja_diferente_maneira', width = 10, title = 'Você costuma comprar da mesma loja de diferentes maneiras?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 20 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>não, costuma fazer minhas compras sempre online        </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>30-45 anos     </td><td>não, costuma fazer minhas compras sempre online        </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>46-60 anos     </td><td>não, costuma fazer minhas compras sempre online        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>não, costuma fazer minhas compras sempre online        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não, costuma fazer minhas compras sempre online        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>32</td><td>12.12</td></tr>
	<tr><td>30-45 anos     </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>11</td><td> 4.17</td></tr>
	<tr><td>46-60 anos     </td><td>não, costumo fazer minhas compras sempre em loja física</td><td>13</td><td> 4.92</td></tr>
	<tr><td>61 anos ou mais</td><td>não, costumo fazer minhas compras sempre em loja física</td><td> 3</td><td> 1.14</td></tr>
	<tr><td>até 17 anos    </td><td>não, costumo fazer minhas compras sempre em loja física</td><td> 4</td><td> 1.52</td></tr>
	<tr><td>18-29 anos     </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>46</td><td>17.42</td></tr>
	<tr><td>30-45 anos     </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>18</td><td> 6.82</td></tr>
	<tr><td>46-60 anos     </td><td>sim, vario dependendo da disponibilidade do produto    </td><td>16</td><td> 6.06</td></tr>
	<tr><td>61 anos ou mais</td><td>sim, vario dependendo da disponibilidade do produto    </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>sim, vario dependendo da disponibilidade do produto    </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>sim, vario dependendo da minha rotina                  </td><td>47</td><td>17.80</td></tr>
	<tr><td>30-45 anos     </td><td>sim, vario dependendo da minha rotina                  </td><td>32</td><td>12.12</td></tr>
	<tr><td>46-60 anos     </td><td>sim, vario dependendo da minha rotina                  </td><td>24</td><td> 9.09</td></tr>
	<tr><td>61 anos ou mais</td><td>sim, vario dependendo da minha rotina                  </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>sim, vario dependendo da minha rotina                  </td><td> 4</td><td> 1.52</td></tr>
</tbody>
</table>




![png](output_40_1.png)



![png](output_40_2.png)



![png](output_40_3.png)



![png](output_40_4.png)


## Faixa Etária X Mais de um meio de compra


```R
ret <- plot_relation('faixa_etaria', 'gosta_mais_meio_compra', width = 12, title = 'Você prefere que as lojas tenha mais de uma maneira de compra?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 25 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>com certeza não                                                                     </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>com certeza não                                                                     </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>com certeza não                                                                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>com certeza não                                                                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>com certeza não                                                                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>55</td><td>20.83</td></tr>
	<tr><td>30-45 anos     </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>23</td><td> 8.71</td></tr>
	<tr><td>46-60 anos     </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td>14</td><td> 5.30</td></tr>
	<tr><td>61 anos ou mais</td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td> 4</td><td> 1.52</td></tr>
	<tr><td>18-29 anos     </td><td>indiferente                                                                         </td><td>26</td><td> 9.85</td></tr>
	<tr><td>30-45 anos     </td><td>indiferente                                                                         </td><td>12</td><td> 4.55</td></tr>
	<tr><td>46-60 anos     </td><td>indiferente                                                                         </td><td>18</td><td> 6.82</td></tr>
	<tr><td>61 anos ou mais</td><td>indiferente                                                                         </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>indiferente                                                                         </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>18-29 anos     </td><td>não                                                                                 </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>não                                                                                 </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>46-60 anos     </td><td>não                                                                                 </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>não                                                                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não                                                                                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>46</td><td>17.42</td></tr>
	<tr><td>30-45 anos     </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>29</td><td>10.98</td></tr>
	<tr><td>46-60 anos     </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>20</td><td> 7.58</td></tr>
	<tr><td>61 anos ou mais</td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>até 17 anos    </td><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td> 2</td><td> 0.76</td></tr>
</tbody>
</table>




![png](output_42_1.png)



![png](output_42_2.png)



![png](output_42_3.png)



![png](output_42_4.png)


## Faixa Etária X Meio de compra (Produto de Beleza)


```R
ret <- plot_relation('faixa_etaria', 'beleza_meio_compra', width = 10, title = 'Meio de Compra (Produtos de Beleza)')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 25 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>na loja física e levar embora na hora da compra</td><td>85</td><td>32.20</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e levar embora na hora da compra</td><td>33</td><td>12.50</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e levar embora na hora da compra</td><td>36</td><td>13.64</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e levar embora na hora da compra</td><td> 5</td><td> 1.89</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e levar embora na hora da compra</td><td> 7</td><td> 2.65</td></tr>
	<tr><td>18-29 anos     </td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não compro este tipo de produto                </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>30-45 anos     </td><td>não compro este tipo de produto                </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>46-60 anos     </td><td>não compro este tipo de produto                </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>61 anos ou mais</td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e ir retirar na loja                    </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>30-45 anos     </td><td>online e ir retirar na loja                    </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>46-60 anos     </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e receber em casa                       </td><td>33</td><td>12.50</td></tr>
	<tr><td>30-45 anos     </td><td>online e receber em casa                       </td><td>26</td><td> 9.85</td></tr>
	<tr><td>46-60 anos     </td><td>online e receber em casa                       </td><td>12</td><td> 4.55</td></tr>
	<tr><td>61 anos ou mais</td><td>online e receber em casa                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e receber em casa                       </td><td> 2</td><td> 0.76</td></tr>
</tbody>
</table>




![png](output_44_1.png)



![png](output_44_2.png)



![png](output_44_3.png)



![png](output_44_4.png)


### Faixa Etária X Meio de Compra (Livros)


```R
ret <- plot_relation('faixa_etaria', 'livro_meio_compra', width = 10, title = 'Meio de Compra (Livros)')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 25 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>na loja física e levar embora na hora da compra</td><td>52</td><td>19.70</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e levar embora na hora da compra</td><td>12</td><td> 4.55</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e levar embora na hora da compra</td><td>28</td><td>10.61</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e levar embora na hora da compra</td><td> 3</td><td> 1.14</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e levar embora na hora da compra</td><td> 5</td><td> 1.89</td></tr>
	<tr><td>18-29 anos     </td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não compro este tipo de produto                </td><td>14</td><td> 5.30</td></tr>
	<tr><td>30-45 anos     </td><td>não compro este tipo de produto                </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>46-60 anos     </td><td>não compro este tipo de produto                </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>30-45 anos     </td><td>online e ir retirar na loja                    </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>46-60 anos     </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>online e ir retirar na loja                    </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e receber em casa                       </td><td>62</td><td>23.48</td></tr>
	<tr><td>30-45 anos     </td><td>online e receber em casa                       </td><td>46</td><td>17.42</td></tr>
	<tr><td>46-60 anos     </td><td>online e receber em casa                       </td><td>22</td><td> 8.33</td></tr>
	<tr><td>61 anos ou mais</td><td>online e receber em casa                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e receber em casa                       </td><td> 4</td><td> 1.52</td></tr>
</tbody>
</table>




![png](output_46_1.png)



![png](output_46_2.png)



![png](output_46_3.png)



![png](output_46_4.png)


## Faixa Etária X Meio de Compra (Alimentação)


```R
ret <- plot_relation('faixa_etaria', 'alimento_meio_compra', width = 10, title = 'Meio de Compra (Alimentação)')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 20 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>na loja física e levar embora na hora da compra</td><td>98</td><td>37.12</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e levar embora na hora da compra</td><td>47</td><td>17.80</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e levar embora na hora da compra</td><td>49</td><td>18.56</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e levar embora na hora da compra</td><td> 5</td><td> 1.89</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e levar embora na hora da compra</td><td> 7</td><td> 2.65</td></tr>
	<tr><td>18-29 anos     </td><td>na loja física e receber na sua casa           </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e receber na sua casa           </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não compro este tipo de produto                </td><td>10</td><td> 3.79</td></tr>
	<tr><td>30-45 anos     </td><td>não compro este tipo de produto                </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>46-60 anos     </td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e receber em casa                       </td><td>15</td><td> 5.68</td></tr>
	<tr><td>30-45 anos     </td><td>online e receber em casa                       </td><td>10</td><td> 3.79</td></tr>
	<tr><td>46-60 anos     </td><td>online e receber em casa                       </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>61 anos ou mais</td><td>online e receber em casa                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e receber em casa                       </td><td> 2</td><td> 0.76</td></tr>
</tbody>
</table>




![png](output_48_1.png)



![png](output_48_2.png)



![png](output_48_3.png)



![png](output_48_4.png)


## Faixa Etária X Meio de Compra (Vestuário)


```R
ret <- plot_relation('faixa_etaria', 'vestuario_meio_compra', width = 10, title = 'Meio de Compra (Vestuário)')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 30 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>                                               </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>                                               </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>                                               </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>                                               </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>                                               </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>na loja física e levar embora na hora da compra</td><td>91</td><td>34.47</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e levar embora na hora da compra</td><td>41</td><td>15.53</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e levar embora na hora da compra</td><td>45</td><td>17.05</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e levar embora na hora da compra</td><td> 6</td><td> 2.27</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e levar embora na hora da compra</td><td> 8</td><td> 3.03</td></tr>
	<tr><td>18-29 anos     </td><td>na loja física e receber na sua casa           </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>30-45 anos     </td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e ir retirar na loja                    </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>30-45 anos     </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>online e ir retirar na loja                    </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e receber em casa                       </td><td>29</td><td>10.98</td></tr>
	<tr><td>30-45 anos     </td><td>online e receber em casa                       </td><td>24</td><td> 9.09</td></tr>
	<tr><td>46-60 anos     </td><td>online e receber em casa                       </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>61 anos ou mais</td><td>online e receber em casa                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e receber em casa                       </td><td> 1</td><td> 0.38</td></tr>
</tbody>
</table>




![png](output_50_1.png)



![png](output_50_2.png)



![png](output_50_3.png)



![png](output_50_4.png)


## Faixa Etária X Meio de Compra (Eletrodomésticos)


```R
ret <- plot_relation('faixa_etaria', 'eletrodomestico_meio_compra', width = 10, title = 'Meio de Compra (Eletrodomésticos)')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 25 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>na loja física e levar embora na hora da compra</td><td>43</td><td>16.29</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e levar embora na hora da compra</td><td>13</td><td> 4.92</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e levar embora na hora da compra</td><td>12</td><td> 4.55</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e levar embora na hora da compra</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e levar embora na hora da compra</td><td> 4</td><td> 1.52</td></tr>
	<tr><td>18-29 anos     </td><td>na loja física e receber na sua casa           </td><td>16</td><td> 6.06</td></tr>
	<tr><td>30-45 anos     </td><td>na loja física e receber na sua casa           </td><td>10</td><td> 3.79</td></tr>
	<tr><td>46-60 anos     </td><td>na loja física e receber na sua casa           </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>na loja física e receber na sua casa           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>na loja física e receber na sua casa           </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não compro este tipo de produto                </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>30-45 anos     </td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>não compro este tipo de produto                </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não compro este tipo de produto                </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>online e ir retirar na loja                    </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>30-45 anos     </td><td>online e ir retirar na loja                    </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>online e ir retirar na loja                    </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>online e ir retirar na loja                    </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>online e receber em casa                       </td><td>63</td><td>23.86</td></tr>
	<tr><td>30-45 anos     </td><td>online e receber em casa                       </td><td>42</td><td>15.91</td></tr>
	<tr><td>46-60 anos     </td><td>online e receber em casa                       </td><td>37</td><td>14.02</td></tr>
	<tr><td>61 anos ou mais</td><td>online e receber em casa                       </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>até 17 anos    </td><td>online e receber em casa                       </td><td> 4</td><td> 1.52</td></tr>
</tbody>
</table>




![png](output_52_1.png)



![png](output_52_2.png)



![png](output_52_3.png)



![png](output_52_4.png)


## Faixa Etária X Meio de Compra (Alimentação)


```R
ret <- plot_relation('faixa_etaria', 'produto_nao_disp_acao', width = 10, title = 'Qual sua ação ao chegar na loja e não encontrar disponibilidade do produto?', nas = T)
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[[4]])
print(ret[[5]])
```


<table>
<caption>A data.frame: 25 × 4</caption>
<thead>
	<tr><th></th><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th></th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>6</th><td>18-29 anos     </td><td>comprar em outra loja                             </td><td>40</td><td>15.15</td></tr>
	<tr><th scope=row>7</th><td>30-45 anos     </td><td>comprar em outra loja                             </td><td>18</td><td> 6.82</td></tr>
	<tr><th scope=row>8</th><td>46-60 anos     </td><td>comprar em outra loja                             </td><td>26</td><td> 9.85</td></tr>
	<tr><th scope=row>9</th><td>61 anos ou mais</td><td>comprar em outra loja                             </td><td> 3</td><td> 1.14</td></tr>
	<tr><th scope=row>10</th><td>até 17 anos    </td><td>comprar em outra loja                             </td><td> 1</td><td> 0.38</td></tr>
	<tr><th scope=row>11</th><td>18-29 anos     </td><td>desistir de comprar                               </td><td> 5</td><td> 1.89</td></tr>
	<tr><th scope=row>12</th><td>30-45 anos     </td><td>desistir de comprar                               </td><td> 6</td><td> 2.27</td></tr>
	<tr><th scope=row>13</th><td>46-60 anos     </td><td>desistir de comprar                               </td><td> 2</td><td> 0.76</td></tr>
	<tr><th scope=row>14</th><td>61 anos ou mais</td><td>desistir de comprar                               </td><td> 0</td><td> 0.00</td></tr>
	<tr><th scope=row>15</th><td>até 17 anos    </td><td>desistir de comprar                               </td><td> 1</td><td> 0.38</td></tr>
	<tr><th scope=row>16</th><td>18-29 anos     </td><td>outro                                             </td><td> 4</td><td> 1.52</td></tr>
	<tr><th scope=row>17</th><td>30-45 anos     </td><td>outro                                             </td><td> 3</td><td> 1.14</td></tr>
	<tr><th scope=row>18</th><td>46-60 anos     </td><td>outro                                             </td><td> 1</td><td> 0.38</td></tr>
	<tr><th scope=row>19</th><td>61 anos ou mais</td><td>outro                                             </td><td> 0</td><td> 0.00</td></tr>
	<tr><th scope=row>20</th><td>até 17 anos    </td><td>outro                                             </td><td> 0</td><td> 0.00</td></tr>
	<tr><th scope=row>21</th><td>18-29 anos     </td><td>realizar a compra na loja e receber na sua casa   </td><td>61</td><td>23.11</td></tr>
	<tr><th scope=row>22</th><td>30-45 anos     </td><td>realizar a compra na loja e receber na sua casa   </td><td>30</td><td>11.36</td></tr>
	<tr><th scope=row>23</th><td>46-60 anos     </td><td>realizar a compra na loja e receber na sua casa   </td><td>19</td><td> 7.20</td></tr>
	<tr><th scope=row>24</th><td>61 anos ou mais</td><td>realizar a compra na loja e receber na sua casa   </td><td> 0</td><td> 0.00</td></tr>
	<tr><th scope=row>25</th><td>até 17 anos    </td><td>realizar a compra na loja e receber na sua casa   </td><td> 4</td><td> 1.52</td></tr>
	<tr><th scope=row>26</th><td>18-29 anos     </td><td>voltar na loja quando o produto estiver disponível</td><td>19</td><td> 7.20</td></tr>
	<tr><th scope=row>27</th><td>30-45 anos     </td><td>voltar na loja quando o produto estiver disponível</td><td> 9</td><td> 3.41</td></tr>
	<tr><th scope=row>28</th><td>46-60 anos     </td><td>voltar na loja quando o produto estiver disponível</td><td> 5</td><td> 1.89</td></tr>
	<tr><th scope=row>29</th><td>61 anos ou mais</td><td>voltar na loja quando o produto estiver disponível</td><td> 3</td><td> 1.14</td></tr>
	<tr><th scope=row>30</th><td>até 17 anos    </td><td>voltar na loja quando o produto estiver disponível</td><td> 3</td><td> 1.14</td></tr>
</tbody>
</table>




![png](output_54_1.png)



![png](output_54_2.png)



![png](output_54_3.png)



![png](output_54_4.png)


## Faixa Etária X Por que compraria online e retiraria fisicamente


```R
ret <- plot_relation('faixa_etaria', 'pq_compra_online_retira_fisico', width = 10, title = 'Por que compraria online e retiraria na loja física?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[4])
print(ret[[5]])
```


<table>
<caption>A data.frame: 35 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>gosto de ir até a loja                                       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>30-45 anos     </td><td>gosto de ir até a loja                                       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>gosto de ir até a loja                                       </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>gosto de ir até a loja                                       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>gosto de ir até a loja                                       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>não escolheria esta opção                                    </td><td>49</td><td>18.56</td></tr>
	<tr><td>30-45 anos     </td><td>não escolheria esta opção                                    </td><td>24</td><td> 9.09</td></tr>
	<tr><td>46-60 anos     </td><td>não escolheria esta opção                                    </td><td>20</td><td> 7.58</td></tr>
	<tr><td>61 anos ou mais</td><td>não escolheria esta opção                                    </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>não escolheria esta opção                                    </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>18-29 anos     </td><td>não tem custo adicional de frete                             </td><td>25</td><td> 9.47</td></tr>
	<tr><td>30-45 anos     </td><td>não tem custo adicional de frete                             </td><td>13</td><td> 4.92</td></tr>
	<tr><td>46-60 anos     </td><td>não tem custo adicional de frete                             </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>61 anos ou mais</td><td>não tem custo adicional de frete                             </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não tem custo adicional de frete                             </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>o produto chega mais rápido                                  </td><td>10</td><td> 3.79</td></tr>
	<tr><td>30-45 anos     </td><td>o produto chega mais rápido                                  </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>46-60 anos     </td><td>o produto chega mais rápido                                  </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>61 anos ou mais</td><td>o produto chega mais rápido                                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>o produto chega mais rápido                                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>posso trocar na mesma hora se não gostar                     </td><td>10</td><td> 3.79</td></tr>
	<tr><td>30-45 anos     </td><td>posso trocar na mesma hora se não gostar                     </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>46-60 anos     </td><td>posso trocar na mesma hora se não gostar                     </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>61 anos ou mais</td><td>posso trocar na mesma hora se não gostar                     </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>posso trocar na mesma hora se não gostar                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>posso ver e provar                                           </td><td>26</td><td> 9.85</td></tr>
	<tr><td>30-45 anos     </td><td>posso ver e provar                                           </td><td>11</td><td> 4.17</td></tr>
	<tr><td>46-60 anos     </td><td>posso ver e provar                                           </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>61 anos ou mais</td><td>posso ver e provar                                           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>posso ver e provar                                           </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td> 8</td><td> 3.03</td></tr>
	<tr><td>30-45 anos     </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td> 4</td><td> 1.52</td></tr>
	<tr><td>46-60 anos     </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td> 5</td><td> 1.89</td></tr>
	<tr><td>61 anos ou mais</td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td> 0</td><td> 0.00</td></tr>
</tbody>
</table>




![png](output_56_1.png)


    [[1]]



![png](output_56_3.png)


    



![png](output_56_5.png)



![png](output_56_6.png)


## Faixa Etária X Por que  compraria na loja física


```R
ret <- plot_relation('faixa_etaria', 'pq_compra_fisica', width = 10, title = 'Por que compraria em loja física?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[4])
print(ret[[5]])
```


<table>
<caption>A data.frame: 35 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>gosto da experiencia de ir até a loja       </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>30-45 anos     </td><td>gosto da experiencia de ir até a loja       </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>gosto da experiencia de ir até a loja       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>gosto da experiencia de ir até a loja       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>gosto da experiencia de ir até a loja       </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>gosto de ver e sentir o produto             </td><td>17</td><td> 6.44</td></tr>
	<tr><td>30-45 anos     </td><td>gosto de ver e sentir o produto             </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>46-60 anos     </td><td>gosto de ver e sentir o produto             </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>61 anos ou mais</td><td>gosto de ver e sentir o produto             </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>gosto de ver e sentir o produto             </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>não escolheria esta opção                   </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>30-45 anos     </td><td>não escolheria esta opção                   </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>46-60 anos     </td><td>não escolheria esta opção                   </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>não escolheria esta opção                   </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não escolheria esta opção                   </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>pela segurança                              </td><td>12</td><td> 4.55</td></tr>
	<tr><td>30-45 anos     </td><td>pela segurança                              </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>pela segurança                              </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>61 anos ou mais</td><td>pela segurança                              </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>pela segurança                              </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por poder experimentar o produto            </td><td>79</td><td>29.92</td></tr>
	<tr><td>30-45 anos     </td><td>por poder experimentar o produto            </td><td>51</td><td>19.32</td></tr>
	<tr><td>46-60 anos     </td><td>por poder experimentar o produto            </td><td>32</td><td>12.12</td></tr>
	<tr><td>61 anos ou mais</td><td>por poder experimentar o produto            </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>até 17 anos    </td><td>por poder experimentar o produto            </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>18-29 anos     </td><td>por poder sair com o produto na hora        </td><td>14</td><td> 5.30</td></tr>
	<tr><td>30-45 anos     </td><td>por poder sair com o produto na hora        </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>46-60 anos     </td><td>por poder sair com o produto na hora        </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>61 anos ou mais</td><td>por poder sair com o produto na hora        </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>por poder sair com o produto na hora        </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por ter um vendedor experiente te orientando</td><td> 3</td><td> 1.14</td></tr>
	<tr><td>30-45 anos     </td><td>por ter um vendedor experiente te orientando</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>por ter um vendedor experiente te orientando</td><td> 3</td><td> 1.14</td></tr>
	<tr><td>61 anos ou mais</td><td>por ter um vendedor experiente te orientando</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por ter um vendedor experiente te orientando</td><td> 0</td><td> 0.00</td></tr>
</tbody>
</table>




![png](output_58_1.png)


    [[1]]



![png](output_58_3.png)


    



![png](output_58_5.png)



![png](output_58_6.png)


## Faixa Etária X Por que compraria online para receber em casa


```R
ret <- plot_relation('faixa_etaria', 'pq_compra_online_recebe_casa', width = 10, title = 'Por que compraria online para receber em casa?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[4])
print(ret[[5]])
```


<table>
<caption>A data.frame: 35 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>não escolheria esta opção        </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>30-45 anos     </td><td>não escolheria esta opção        </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>46-60 anos     </td><td>não escolheria esta opção        </td><td> 7</td><td> 2.65</td></tr>
	<tr><td>61 anos ou mais</td><td>não escolheria esta opção        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>não escolheria esta opção        </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>não tenho tempo de ir até a loja </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>30-45 anos     </td><td>não tenho tempo de ir até a loja </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>46-60 anos     </td><td>não tenho tempo de ir até a loja </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>61 anos ou mais</td><td>não tenho tempo de ir até a loja </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>não tenho tempo de ir até a loja </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>pela comodidade                  </td><td>80</td><td>30.30</td></tr>
	<tr><td>30-45 anos     </td><td>pela comodidade                  </td><td>42</td><td>15.91</td></tr>
	<tr><td>46-60 anos     </td><td>pela comodidade                  </td><td>32</td><td>12.12</td></tr>
	<tr><td>61 anos ou mais</td><td>pela comodidade                  </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>até 17 anos    </td><td>pela comodidade                  </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>18-29 anos     </td><td>pela rapidez                     </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>30-45 anos     </td><td>pela rapidez                     </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>pela rapidez                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>pela rapidez                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>pela rapidez                     </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por só ter certos produtos online</td><td> 7</td><td> 2.65</td></tr>
	<tr><td>30-45 anos     </td><td>por só ter certos produtos online</td><td> 2</td><td> 0.76</td></tr>
	<tr><td>46-60 anos     </td><td>por só ter certos produtos online</td><td> 2</td><td> 0.76</td></tr>
	<tr><td>61 anos ou mais</td><td>por só ter certos produtos online</td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por só ter certos produtos online</td><td> 2</td><td> 0.76</td></tr>
	<tr><td>18-29 anos     </td><td>por ter preço mais baixo         </td><td>19</td><td> 7.20</td></tr>
	<tr><td>30-45 anos     </td><td>por ter preço mais baixo         </td><td>11</td><td> 4.17</td></tr>
	<tr><td>46-60 anos     </td><td>por ter preço mais baixo         </td><td> 9</td><td> 3.41</td></tr>
	<tr><td>61 anos ou mais</td><td>por ter preço mais baixo         </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>por ter preço mais baixo         </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por ter promoção                 </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>30-45 anos     </td><td>por ter promoção                 </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>46-60 anos     </td><td>por ter promoção                 </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>61 anos ou mais</td><td>por ter promoção                 </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por ter promoção                 </td><td> 1</td><td> 0.38</td></tr>
</tbody>
</table>




![png](output_60_1.png)


    [[1]]



![png](output_60_3.png)


    



![png](output_60_5.png)



![png](output_60_6.png)


## Faixa Etária X Por que compraria na loja física para receber em casa


```R
ret <- plot_relation('faixa_etaria', 'pq_compra_fisica_recebe_casa', width = 10, title = 'Por que compraria na loja física para receber em casa?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[4])
print(ret[[5]])
```


<table>
<caption>A data.frame: 35 × 4</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Var2</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>18-29 anos     </td><td>não escolheria esta opção                           </td><td>61</td><td>23.11</td></tr>
	<tr><td>30-45 anos     </td><td>não escolheria esta opção                           </td><td>25</td><td> 9.47</td></tr>
	<tr><td>46-60 anos     </td><td>não escolheria esta opção                           </td><td>23</td><td> 8.71</td></tr>
	<tr><td>61 anos ou mais</td><td>não escolheria esta opção                           </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>até 17 anos    </td><td>não escolheria esta opção                           </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>18-29 anos     </td><td>pela facilidade de achar o produto                  </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>30-45 anos     </td><td>pela facilidade de achar o produto                  </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>46-60 anos     </td><td>pela facilidade de achar o produto                  </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>61 anos ou mais</td><td>pela facilidade de achar o produto                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>pela facilidade de achar o produto                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>pela praticidade                                    </td><td>26</td><td> 9.85</td></tr>
	<tr><td>30-45 anos     </td><td>pela praticidade                                    </td><td>12</td><td> 4.55</td></tr>
	<tr><td>46-60 anos     </td><td>pela praticidade                                    </td><td> 6</td><td> 2.27</td></tr>
	<tr><td>61 anos ou mais</td><td>pela praticidade                                    </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>até 17 anos    </td><td>pela praticidade                                    </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>por não precisa voltar na loja para buscar o produto</td><td> 7</td><td> 2.65</td></tr>
	<tr><td>30-45 anos     </td><td>por não precisa voltar na loja para buscar o produto</td><td>13</td><td> 4.92</td></tr>
	<tr><td>46-60 anos     </td><td>por não precisa voltar na loja para buscar o produto</td><td> 6</td><td> 2.27</td></tr>
	<tr><td>61 anos ou mais</td><td>por não precisa voltar na loja para buscar o produto</td><td> 1</td><td> 0.38</td></tr>
	<tr><td>até 17 anos    </td><td>por não precisa voltar na loja para buscar o produto</td><td> 1</td><td> 0.38</td></tr>
	<tr><td>18-29 anos     </td><td>por nao precisar carregar os produtos               </td><td>21</td><td> 7.95</td></tr>
	<tr><td>30-45 anos     </td><td>por nao precisar carregar os produtos               </td><td> 9</td><td> 3.41</td></tr>
	<tr><td>46-60 anos     </td><td>por nao precisar carregar os produtos               </td><td>12</td><td> 4.55</td></tr>
	<tr><td>61 anos ou mais</td><td>por nao precisar carregar os produtos               </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por nao precisar carregar os produtos               </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>18-29 anos     </td><td>por poder personalizar meu produto                  </td><td> 5</td><td> 1.89</td></tr>
	<tr><td>30-45 anos     </td><td>por poder personalizar meu produto                  </td><td> 1</td><td> 0.38</td></tr>
	<tr><td>46-60 anos     </td><td>por poder personalizar meu produto                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>61 anos ou mais</td><td>por poder personalizar meu produto                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por poder personalizar meu produto                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>18-29 anos     </td><td>por receber orientacao do vendedor                  </td><td> 4</td><td> 1.52</td></tr>
	<tr><td>30-45 anos     </td><td>por receber orientacao do vendedor                  </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>46-60 anos     </td><td>por receber orientacao do vendedor                  </td><td> 2</td><td> 0.76</td></tr>
	<tr><td>61 anos ou mais</td><td>por receber orientacao do vendedor                  </td><td> 0</td><td> 0.00</td></tr>
	<tr><td>até 17 anos    </td><td>por receber orientacao do vendedor                  </td><td> 0</td><td> 0.00</td></tr>
</tbody>
</table>




![png](output_62_1.png)


    [[1]]



![png](output_62_3.png)


    



![png](output_62_5.png)



![png](output_62_6.png)



```R
ret <- plot_relation_multi('faixa_etaria', 'impedimento_compra_online', width = 10, title = 'Qual seu maior impedimento para comprar online?')
ret[[1]]
print(ret[[2]])
print(ret[[3]])
print(ret[4])
print(ret[[5]])
```


<table>
<caption>A data.frame: 55 × 3</caption>
<thead>
	<tr><th scope=col>impedimento_compra_online</th><th scope=col>Proporcao</th><th scope=col>faixa_etaria</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><td>a demora para receber o produto                          </td><td> 8.0</td><td>18-29 anos     </td></tr>
	<tr><td>a falta de informações sobre o produto                   </td><td> 8.6</td><td>18-29 anos     </td></tr>
	<tr><td>a falta de padronização dos tamanhos                     </td><td> 6.8</td><td>18-29 anos     </td></tr>
	<tr><td>a incerteza se o produto ira servir                      </td><td>17.9</td><td>18-29 anos     </td></tr>
	<tr><td>a insegurança em relacao ao pagamento                    </td><td> 5.6</td><td>18-29 anos     </td></tr>
	<tr><td>a inseguranca se o poduto realmente chegara              </td><td> 5.6</td><td>18-29 anos     </td></tr>
	<tr><td>gostar da experiencia que tenho quando vou na loja física</td><td> 7.4</td><td>18-29 anos     </td></tr>
	<tr><td>nada me impede de comprar online                         </td><td> 7.4</td><td>18-29 anos     </td></tr>
	<tr><td>não poder experimentar o produto                         </td><td>19.8</td><td>18-29 anos     </td></tr>
	<tr><td>não poder ver e tocar o produto                          </td><td>11.7</td><td>18-29 anos     </td></tr>
	<tr><td>outros                                                   </td><td> 1.2</td><td>18-29 anos     </td></tr>
	<tr><td>a demora para receber o produto                          </td><td> 3.1</td><td>30-45 anos     </td></tr>
	<tr><td>a falta de informações sobre o produto                   </td><td> 4.6</td><td>30-45 anos     </td></tr>
	<tr><td>a falta de padronização dos tamanhos                     </td><td> 9.2</td><td>30-45 anos     </td></tr>
	<tr><td>a incerteza se o produto ira servir                      </td><td>16.9</td><td>30-45 anos     </td></tr>
	<tr><td>a insegurança em relacao ao pagamento                    </td><td> 0.0</td><td>30-45 anos     </td></tr>
	<tr><td>a inseguranca se o poduto realmente chegara              </td><td> 6.2</td><td>30-45 anos     </td></tr>
	<tr><td>gostar da experiencia que tenho quando vou na loja física</td><td> 6.2</td><td>30-45 anos     </td></tr>
	<tr><td>nada me impede de comprar online                         </td><td>15.4</td><td>30-45 anos     </td></tr>
	<tr><td>não poder experimentar o produto                         </td><td>24.6</td><td>30-45 anos     </td></tr>
	<tr><td>não poder ver e tocar o produto                          </td><td>10.8</td><td>30-45 anos     </td></tr>
	<tr><td>outros                                                   </td><td> 3.1</td><td>30-45 anos     </td></tr>
	<tr><td>a demora para receber o produto                          </td><td> 2.3</td><td>46-60 anos     </td></tr>
	<tr><td>a falta de informações sobre o produto                   </td><td> 6.8</td><td>46-60 anos     </td></tr>
	<tr><td>a falta de padronização dos tamanhos                     </td><td> 6.8</td><td>46-60 anos     </td></tr>
	<tr><td>a incerteza se o produto ira servir                      </td><td>29.5</td><td>46-60 anos     </td></tr>
	<tr><td>a insegurança em relacao ao pagamento                    </td><td> 4.5</td><td>46-60 anos     </td></tr>
	<tr><td>a inseguranca se o poduto realmente chegara              </td><td> 9.1</td><td>46-60 anos     </td></tr>
	<tr><td>gostar da experiencia que tenho quando vou na loja física</td><td> 4.5</td><td>46-60 anos     </td></tr>
	<tr><td>nada me impede de comprar online                         </td><td>13.6</td><td>46-60 anos     </td></tr>
	<tr><td>não poder experimentar o produto                         </td><td>13.6</td><td>46-60 anos     </td></tr>
	<tr><td>não poder ver e tocar o produto                          </td><td> 9.1</td><td>46-60 anos     </td></tr>
	<tr><td>outros                                                   </td><td> 0.0</td><td>46-60 anos     </td></tr>
	<tr><td>a demora para receber o produto                          </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>a falta de informações sobre o produto                   </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>a falta de padronização dos tamanhos                     </td><td>25.0</td><td>61 anos ou mais</td></tr>
	<tr><td>a incerteza se o produto ira servir                      </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>a insegurança em relacao ao pagamento                    </td><td>25.0</td><td>61 anos ou mais</td></tr>
	<tr><td>a inseguranca se o poduto realmente chegara              </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>gostar da experiencia que tenho quando vou na loja física</td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>nada me impede de comprar online                         </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>não poder experimentar o produto                         </td><td>25.0</td><td>61 anos ou mais</td></tr>
	<tr><td>não poder ver e tocar o produto                          </td><td>25.0</td><td>61 anos ou mais</td></tr>
	<tr><td>outros                                                   </td><td> 0.0</td><td>61 anos ou mais</td></tr>
	<tr><td>a demora para receber o produto                          </td><td>22.2</td><td>até 17 anos    </td></tr>
	<tr><td>a falta de informações sobre o produto                   </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>a falta de padronização dos tamanhos                     </td><td>11.1</td><td>até 17 anos    </td></tr>
	<tr><td>a incerteza se o produto ira servir                      </td><td>22.2</td><td>até 17 anos    </td></tr>
	<tr><td>a insegurança em relacao ao pagamento                    </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>a inseguranca se o poduto realmente chegara              </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>gostar da experiencia que tenho quando vou na loja física</td><td>22.2</td><td>até 17 anos    </td></tr>
	<tr><td>nada me impede de comprar online                         </td><td>11.1</td><td>até 17 anos    </td></tr>
	<tr><td>não poder experimentar o produto                         </td><td> 0.0</td><td>até 17 anos    </td></tr>
	<tr><td>não poder ver e tocar o produto                          </td><td>11.1</td><td>até 17 anos    </td></tr>
	<tr><td>outros                                                   </td><td> 0.0</td><td>até 17 anos    </td></tr>
</tbody>
</table>




![png](output_63_1.png)


    [[1]]



![png](output_63_3.png)


    



![png](output_63_5.png)



![png](output_63_6.png)



```R

```


```R

```


```R

```


```R

```


```R

```

---

# Gênero


```R
plot_duda('genero', reorder = T, title = 'Gênero')
```


<table>
<caption>A data.frame: 2 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>feminino </td><td>174</td><td>65.91</td></tr>
	<tr><td>masculino</td><td> 90</td><td>34.09</td></tr>
</tbody>
</table>




![png](output_70_1.png)



```R
with(duda, table(genero))
```


    genero
     feminino masculino 
          174        90 



```R
plot_duda('regiao', reorder = T, title = 'Região')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>Curitiba                </td><td>237</td><td>89.77</td></tr>
	<tr><td>exterior                </td><td>  6</td><td> 2.27</td></tr>
	<tr><td>Fora do estado do Paraná</td><td>  5</td><td> 1.89</td></tr>
	<tr><td>Outra cidade do Paraná  </td><td>  8</td><td> 3.03</td></tr>
	<tr><td>Região Metropolitana    </td><td>  8</td><td> 3.03</td></tr>
</tbody>
</table>




![png](output_72_1.png)



```R
plot_duda('ocupacao', reorder = T, width = 10, title = 'Ocupação')
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>aposentado                      </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>desempregado no momento         </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>empreendedor/ empresário        </td><td>52</td><td>19.70</td></tr>
	<tr><td>empregado de empresa privada    </td><td>54</td><td>20.45</td></tr>
	<tr><td>estudante                       </td><td>86</td><td>32.58</td></tr>
	<tr><td>funcionário publico             </td><td>12</td><td> 4.55</td></tr>
	<tr><td>profissional liberal ou autônomo</td><td>54</td><td>20.45</td></tr>
</tbody>
</table>




![png](output_73_1.png)



```R
plot_duda('renda_familiar', levels = c('até 2.000', '2.000 a 5.000', '5.000 a 10.000', '10.000 a 15.000', 'mais de 15.000'), title = 'Renda Familiar')
```

    Warning message in if (levels == T) {:
    “the condition has length > 1 and only the first element will be used”


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>10.000 a 15.000</td><td> 37</td><td>14.02</td></tr>
	<tr><td>2.000 a 5.000  </td><td> 17</td><td> 6.44</td></tr>
	<tr><td>5.000 a 10.000 </td><td> 35</td><td>13.26</td></tr>
	<tr><td>até 2.000      </td><td>  5</td><td> 1.89</td></tr>
	<tr><td>mais de 15.000 </td><td>170</td><td>64.39</td></tr>
</tbody>
</table>




![png](output_74_2.png)



```R
plot_duda('gosta_compras', title = 'Gosta de fazer compras?')
```


<table>
<caption>A data.frame: 2 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>não</td><td> 20</td><td> 7.58</td></tr>
	<tr><td>sim</td><td>244</td><td>92.42</td></tr>
</tbody>
</table>




![png](output_75_1.png)



```R
plot_duda('dificuldade_compra', width = 10, title = 'Dificuldade de Compra')
```


<table>
<caption>A data.frame: 8 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>distância grande das lojas que gosto     </td><td>14</td><td> 5.30</td></tr>
	<tr><td>falta de estacionamento nos locais       </td><td>17</td><td> 6.44</td></tr>
	<tr><td>falta de tempo                           </td><td>90</td><td>34.09</td></tr>
	<tr><td>não gosto de ir ao supermercado          </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>não gosto de ir em shoppings             </td><td>17</td><td> 6.44</td></tr>
	<tr><td>não gosto de ter contato com vendedores  </td><td>10</td><td> 3.79</td></tr>
	<tr><td>não tenho dificuldades para fazer compras</td><td>59</td><td>22.35</td></tr>
	<tr><td>tenho preguiça de ir até as lojas        </td><td>54</td><td>20.45</td></tr>
</tbody>
</table>




![png](output_76_1.png)



```R
plot_multi('tipo_loja', reorder = T, width = 10, title = 'Tipo de Loja')
```


<table>
<caption>A data.frame: 10 × 3</caption>
<thead>
	<tr><th scope=col>value</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>brinquedos          </td><td> 15</td><td> 2.06</td></tr>
	<tr><td>eletrodomêsticos    </td><td>  8</td><td> 1.10</td></tr>
	<tr><td>esportes            </td><td> 74</td><td>10.15</td></tr>
	<tr><td>livrarias           </td><td> 66</td><td> 9.05</td></tr>
	<tr><td>móveis/ decoração   </td><td> 35</td><td> 4.80</td></tr>
	<tr><td>outros              </td><td> 22</td><td> 3.02</td></tr>
	<tr><td>produtos eletrônicos</td><td> 43</td><td> 5.90</td></tr>
	<tr><td>roupas              </td><td>230</td><td>31.55</td></tr>
	<tr><td>sapatos             </td><td>127</td><td>17.42</td></tr>
	<tr><td>supermercado        </td><td>109</td><td>14.95</td></tr>
</tbody>
</table>




![png](output_77_1.png)



```R
plot_multi('fonte', reorder = T, width = 11, title = 'Fonte de Informação')
```


<table>
<caption>A data.frame: 12 × 3</caption>
<thead>
	<tr><th scope=col>value</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>amigos/ colegas                    </td><td> 89</td><td>10.80</td></tr>
	<tr><td>blogueiras/os                      </td><td> 67</td><td> 8.13</td></tr>
	<tr><td>email marketing                    </td><td> 14</td><td> 1.70</td></tr>
	<tr><td>outros                             </td><td>  4</td><td> 0.49</td></tr>
	<tr><td>pesquisando na internet            </td><td>150</td><td>18.20</td></tr>
	<tr><td>propagandas em revista/jornal      </td><td> 15</td><td> 1.82</td></tr>
	<tr><td>propagandas redes sociais          </td><td>145</td><td>17.60</td></tr>
	<tr><td>seguindo as lojas nas redes sociais</td><td>144</td><td>17.48</td></tr>
	<tr><td>sites de promoção                  </td><td> 45</td><td> 5.46</td></tr>
	<tr><td>televisão                          </td><td> 11</td><td> 1.33</td></tr>
	<tr><td>vitrines                           </td><td> 97</td><td>11.77</td></tr>
	<tr><td>WhatsApp                           </td><td> 43</td><td> 5.22</td></tr>
</tbody>
</table>




![png](output_78_1.png)



```R
plot_multi('consid', reorder = T, title = 'O que você leva em consideração na hora de escolher uma loja?', width = 11)
```


<table>
<caption>A data.frame: 12 × 3</caption>
<thead>
	<tr><th scope=col>value</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>confiança na marca/produto                    </td><td>129</td><td>12.26</td></tr>
	<tr><td>diferentes opções de entrega do produto       </td><td> 19</td><td> 1.81</td></tr>
	<tr><td>experiencia de compra na loja                 </td><td>104</td><td> 9.89</td></tr>
	<tr><td>informações sobre o produto                   </td><td> 76</td><td> 7.22</td></tr>
	<tr><td>localização da loja                           </td><td> 89</td><td> 8.46</td></tr>
	<tr><td>padronização do produto em relação a numeração</td><td> 24</td><td> 2.28</td></tr>
	<tr><td>presença nas mídias sociais                   </td><td> 24</td><td> 2.28</td></tr>
	<tr><td>produto personalizado/ exclusivo              </td><td> 47</td><td> 4.47</td></tr>
	<tr><td>promoção                                      </td><td>125</td><td>11.88</td></tr>
	<tr><td>qualidade do atendimento                      </td><td>121</td><td>11.50</td></tr>
	<tr><td>qualidade do produto                          </td><td>234</td><td>22.24</td></tr>
	<tr><td>venda online                                  </td><td> 60</td><td> 5.70</td></tr>
</tbody>
</table>




![png](output_79_1.png)



```R
plot_multi('relac', reorder = T, title = 'O que você leva em consideração na hora de escolher uma loja?', width = 10)
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>value</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>abro os emails marketing que recebo das lojas que gosto                  </td><td> 39</td><td> 7.34</td></tr>
	<tr><td>estou sempre visitando o site das lojas que gosto                        </td><td>141</td><td>26.55</td></tr>
	<tr><td>não sigo as lojas que gosto  mas acompanho o seu perfil nas redes sociais</td><td> 32</td><td> 6.03</td></tr>
	<tr><td>sempre entro em contato com o vendedor da loja pelo WhatsApp             </td><td> 35</td><td> 6.59</td></tr>
	<tr><td>sigo as lojas que gosto nas redes sociais                                </td><td>161</td><td>30.32</td></tr>
	<tr><td>tenho o aplicativo das lojas que gosto                                   </td><td> 43</td><td> 8.10</td></tr>
	<tr><td>vejo propaganda nas redes sociais                                        </td><td> 80</td><td>15.07</td></tr>
</tbody>
</table>




![png](output_80_1.png)



```R
plot_duda('meio_preferido', reorder = T, title = 'Qual o seu meio preferido para fazer compras?', width = 9)
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>loja física                                  </td><td>181</td><td>68.56</td></tr>
	<tr><td>outro                                        </td><td>  3</td><td> 1.14</td></tr>
	<tr><td>por aplicativo                               </td><td>  9</td><td> 3.41</td></tr>
	<tr><td>por catálogo                                 </td><td>  2</td><td> 0.76</td></tr>
	<tr><td>por telefone                                 </td><td>  1</td><td> 0.38</td></tr>
	<tr><td>redes sociais (Instagram, facebook, WhatsApp)</td><td> 17</td><td> 6.44</td></tr>
	<tr><td>site da loja                                 </td><td> 51</td><td>19.32</td></tr>
</tbody>
</table>




![png](output_81_1.png)



```R
plot_duda('papel_internet', reorder = T, title = 'Qual o papel da internet na hora de fazer compras?', width = 10)
```


<table>
<caption>A data.frame: 8 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>é na internet que vejo propagandas e promoções que me levam a compra</td><td>41</td><td>15.53</td></tr>
	<tr><td>faço pesquisa de preço  na internet                                 </td><td>51</td><td>19.32</td></tr>
	<tr><td>não uso a internet antes de fazer uma compra                        </td><td> 8</td><td> 3.03</td></tr>
	<tr><td>pesquiso informações sobre produtos na internet                     </td><td>39</td><td>14.77</td></tr>
	<tr><td>saber as novidades das lojas                                        </td><td>85</td><td>32.20</td></tr>
	<tr><td>uso a internet somente para realizar a compra                       </td><td> 3</td><td> 1.14</td></tr>
	<tr><td>utilizo a internet em todo meu processo de compra                   </td><td>32</td><td>12.12</td></tr>
	<tr><td>vejo avaliações de lojas na internet                                </td><td> 5</td><td> 1.89</td></tr>
</tbody>
</table>




![png](output_82_1.png)



```R
plot_duda('faz_compra_online', reorder = T, title = 'Você costuma fazer compras online?', width = 9)
```


<table>
<caption>A data.frame: 3 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>não, não gosto de comprar online             </td><td> 38</td><td>14.39</td></tr>
	<tr><td>sim, faço a maioria das minhas compras online</td><td> 46</td><td>17.42</td></tr>
	<tr><td>sim, faço algumas compras online             </td><td>180</td><td>68.18</td></tr>
</tbody>
</table>




![png](output_83_1.png)



```R
plot_multi('tipos_prod', reorder = T, width = 10, title = 'Que tipo de produtos você costuma comprar online?')
```


<table>
<caption>A data.frame: 15 × 3</caption>
<thead>
	<tr><th scope=col>value</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>acessórios                        </td><td> 94</td><td> 8.42</td></tr>
	<tr><td>eletrodomêsticos                  </td><td> 58</td><td> 5.20</td></tr>
	<tr><td>eletrónicos                       </td><td> 98</td><td> 8.78</td></tr>
	<tr><td>fármacia                          </td><td> 32</td><td> 2.87</td></tr>
	<tr><td>filmes                            </td><td> 21</td><td> 1.88</td></tr>
	<tr><td>ingressos de cinema/ shows/ teatro</td><td>148</td><td>13.26</td></tr>
	<tr><td>livros                            </td><td> 74</td><td> 6.63</td></tr>
	<tr><td>não compro online                 </td><td> 18</td><td> 1.61</td></tr>
	<tr><td>outros                            </td><td>  8</td><td> 0.72</td></tr>
	<tr><td>passagem aérea                    </td><td>171</td><td>15.32</td></tr>
	<tr><td>produtos de beleza                </td><td> 79</td><td> 7.08</td></tr>
	<tr><td>produtos esportivos               </td><td> 44</td><td> 3.94</td></tr>
	<tr><td>roupas                            </td><td>151</td><td>13.53</td></tr>
	<tr><td>sapato                            </td><td>102</td><td> 9.14</td></tr>
	<tr><td>supermercado                      </td><td> 18</td><td> 1.61</td></tr>
</tbody>
</table>




![png](output_84_1.png)



```R
plot_duda('mesma_loja_diferente_maneira', reorder = T, width = 9, title = 'Você costuma comprar da mesma loja de diferentes maneiras?')
```


<table>
<caption>A data.frame: 4 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>não, costuma fazer minhas compras sempre online        </td><td> 10</td><td> 3.79</td></tr>
	<tr><td>não, costumo fazer minhas compras sempre em loja física</td><td> 63</td><td>23.86</td></tr>
	<tr><td>sim, vario dependendo da disponibilidade do produto    </td><td> 83</td><td>31.44</td></tr>
	<tr><td>sim, vario dependendo da minha rotina                  </td><td>108</td><td>40.91</td></tr>
</tbody>
</table>




![png](output_85_1.png)



```R
plot_duda('gosta_mais_meio_compra', reorder = T, width = 12, title = 'Você prefere que as lojas tenham mais de uma forma de compra?')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>com certeza não                                                                     </td><td>  2</td><td> 0.76</td></tr>
	<tr><td>com certeza sim, seria demais a loja me oferecer diversas opções de compra e entrega</td><td> 97</td><td>36.74</td></tr>
	<tr><td>indiferente                                                                         </td><td> 61</td><td>23.11</td></tr>
	<tr><td>não                                                                                 </td><td>  4</td><td> 1.52</td></tr>
	<tr><td>sim, seria bom ter diferentes opções de compra e entrega                            </td><td>100</td><td>37.88</td></tr>
</tbody>
</table>




![png](output_86_1.png)



```R
plot_duda('beleza_meio_compra', reorder = T, width = 10, title = 'Meios de Compra (Produtos de Beleza)')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>na loja física e levar embora na hora da compra</td><td>166</td><td>62.88</td></tr>
	<tr><td>na loja física e receber na sua casa           </td><td>  2</td><td> 0.76</td></tr>
	<tr><td>não compro este tipo de produto                </td><td> 19</td><td> 7.20</td></tr>
	<tr><td>online e ir retirar na loja                    </td><td>  4</td><td> 1.52</td></tr>
	<tr><td>online e receber em casa                       </td><td> 73</td><td>27.65</td></tr>
</tbody>
</table>




![png](output_87_1.png)



```R
plot_duda('livro_meio_compra', reorder = T, width = 10, title = 'Meios de Compra (Produtos de Beleza)')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>na loja física e levar embora na hora da compra</td><td>100</td><td>37.88</td></tr>
	<tr><td>na loja física e receber na sua casa           </td><td>  2</td><td> 0.76</td></tr>
	<tr><td>não compro este tipo de produto                </td><td> 22</td><td> 8.33</td></tr>
	<tr><td>online e ir retirar na loja                    </td><td>  6</td><td> 2.27</td></tr>
	<tr><td>online e receber em casa                       </td><td>134</td><td>50.76</td></tr>
</tbody>
</table>




![png](output_88_1.png)



```R
colnames(duda)
```


<ol class=list-inline>
	<li>'ID'</li>
	<li>'date'</li>
	<li>'duration'</li>
	<li>'faixa_etaria'</li>
	<li>'genero'</li>
	<li>'escolaridade'</li>
	<li>'regiao'</li>
	<li>'ocupacao'</li>
	<li>'renda_familiar'</li>
	<li>'gosta_compras'</li>
	<li>'dificuldade_compra'</li>
	<li>'tipo_loja'</li>
	<li>'fonte_informacao'</li>
	<li>'consideracao_loja'</li>
	<li>'relacao_loja_online'</li>
	<li>'meio_preferido'</li>
	<li>'papel_internet'</li>
	<li>'faz_compra_online'</li>
	<li>'tipos_prod_online'</li>
	<li>'mesma_loja_diferente_maneira'</li>
	<li>'gosta_mais_meio_compra'</li>
	<li>'beleza_meio_compra'</li>
	<li>'livro_meio_compra'</li>
	<li>'alimento_meio_compra'</li>
	<li>'vestuario_meio_compra'</li>
	<li>'eletrodomestico_meio_compra'</li>
	<li>'produto_nao_disp_acao'</li>
	<li>'pq_compra_online_retira_fisico'</li>
	<li>'pq_compra_fisica'</li>
	<li>'pq_compra_online_recebe_casa'</li>
	<li>'pq_compra_fisica_recebe_casa'</li>
	<li>'impedimento_compra_online'</li>
</ol>




```R
plot_duda('alimento_meio_compra', reorder = T, width = 9, title = 'Meios de Compra (Alimentação)')
```


<table>
<caption>A data.frame: 4 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>na loja física e levar embora na hora da compra</td><td>206</td><td>78.03</td></tr>
	<tr><td>na loja física e receber na sua casa           </td><td> 13</td><td> 4.92</td></tr>
	<tr><td>não compro este tipo de produto                </td><td> 16</td><td> 6.06</td></tr>
	<tr><td>online e receber em casa                       </td><td> 29</td><td>10.98</td></tr>
</tbody>
</table>




![png](output_90_1.png)



```R
plot_duda('vestuario_meio_compra', reorder = T, width = 10, title = 'Meios de Compra (Vestuário)')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th></th><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th></th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>2</th><td>na loja física e levar embora na hora da compra</td><td>191</td><td>72.35</td></tr>
	<tr><th scope=row>3</th><td>na loja física e receber na sua casa           </td><td>  5</td><td> 1.89</td></tr>
	<tr><th scope=row>4</th><td>não compro este tipo de produto                </td><td>  1</td><td> 0.38</td></tr>
	<tr><th scope=row>5</th><td>online e ir retirar na loja                    </td><td>  5</td><td> 1.89</td></tr>
	<tr><th scope=row>6</th><td>online e receber em casa                       </td><td> 61</td><td>23.11</td></tr>
</tbody>
</table>




![png](output_91_1.png)



```R
plot_duda('eletrodomestico_meio_compra', reorder = T, width = 10, title = 'Meios de Compra (Eletrodomésticos)')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>na loja física e levar embora na hora da compra</td><td> 72</td><td>27.27</td></tr>
	<tr><td>na loja física e receber na sua casa           </td><td> 30</td><td>11.36</td></tr>
	<tr><td>não compro este tipo de produto                </td><td>  7</td><td> 2.65</td></tr>
	<tr><td>online e ir retirar na loja                    </td><td>  4</td><td> 1.52</td></tr>
	<tr><td>online e receber em casa                       </td><td>151</td><td>57.20</td></tr>
</tbody>
</table>




![png](output_92_1.png)



```R
plot_duda('produto_nao_disp_acao', reorder = T, width = 10, title = 'Ação ao encontrar produto não disponível')
```


<table>
<caption>A data.frame: 5 × 3</caption>
<thead>
	<tr><th></th><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th></th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>2</th><td>comprar em outra loja                             </td><td> 88</td><td>33.33</td></tr>
	<tr><th scope=row>3</th><td>desistir de comprar                               </td><td> 14</td><td> 5.30</td></tr>
	<tr><th scope=row>4</th><td>outro                                             </td><td>  8</td><td> 3.03</td></tr>
	<tr><th scope=row>5</th><td>realizar a compra na loja e receber na sua casa   </td><td>114</td><td>43.18</td></tr>
	<tr><th scope=row>6</th><td>voltar na loja quando o produto estiver disponível</td><td> 39</td><td>14.77</td></tr>
</tbody>
</table>




![png](output_93_1.png)



```R
plot_duda('pq_compra_fisica', reorder = T, width = 10, title = 'Por que compraria em loja física?')
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>gosto da experiencia de ir até a loja       </td><td>  3</td><td> 1.14</td></tr>
	<tr><td>gosto de ver e sentir o produto             </td><td> 26</td><td> 9.85</td></tr>
	<tr><td>não escolheria esta opção                   </td><td>  9</td><td> 3.41</td></tr>
	<tr><td>pela segurança                              </td><td> 16</td><td> 6.06</td></tr>
	<tr><td>por poder experimentar o produto            </td><td>173</td><td>65.53</td></tr>
	<tr><td>por poder sair com o produto na hora        </td><td> 31</td><td>11.74</td></tr>
	<tr><td>por ter um vendedor experiente te orientando</td><td>  6</td><td> 2.27</td></tr>
</tbody>
</table>




![png](output_94_1.png)



```R
plot_duda('pq_compra_online_retira_fisico', reorder = T, width = 10, title = 'Por que compraria online e retiraria na loja física?')
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>gosto de ir até a loja                                       </td><td>  6</td><td> 2.27</td></tr>
	<tr><td>não escolheria esta opção                                    </td><td>102</td><td>38.64</td></tr>
	<tr><td>não tem custo adicional de frete                             </td><td> 44</td><td>16.67</td></tr>
	<tr><td>o produto chega mais rápido                                  </td><td> 25</td><td> 9.47</td></tr>
	<tr><td>posso trocar na mesma hora se não gostar                     </td><td> 22</td><td> 8.33</td></tr>
	<tr><td>posso ver e provar                                           </td><td> 47</td><td>17.80</td></tr>
	<tr><td>tenho certeza de que o produto que eu quero vai estar na loja</td><td> 18</td><td> 6.82</td></tr>
</tbody>
</table>




![png](output_95_1.png)



```R
plot_duda('pq_compra_online_recebe_casa', reorder = T, width = 10, title = 'Por que compraria online para receber em casa?')
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>não escolheria esta opção        </td><td> 23</td><td> 8.71</td></tr>
	<tr><td>não tenho tempo de ir até a loja </td><td>  4</td><td> 1.52</td></tr>
	<tr><td>pela comodidade                  </td><td>162</td><td>61.36</td></tr>
	<tr><td>pela rapidez                     </td><td>  6</td><td> 2.27</td></tr>
	<tr><td>por só ter certos produtos online</td><td> 13</td><td> 4.92</td></tr>
	<tr><td>por ter preço mais baixo         </td><td> 41</td><td>15.53</td></tr>
	<tr><td>por ter promoção                 </td><td> 15</td><td> 5.68</td></tr>
</tbody>
</table>




![png](output_96_1.png)



```R
plot_duda('pq_compra_fisica_recebe_casa', reorder = T, width = 10, title = 'Por que compraria online para receber em casa?')
```


<table>
<caption>A data.frame: 7 × 3</caption>
<thead>
	<tr><th scope=col>Var1</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>não escolheria esta opção                           </td><td>117</td><td>44.32</td></tr>
	<tr><td>pela facilidade de achar o produto                  </td><td> 13</td><td> 4.92</td></tr>
	<tr><td>pela praticidade                                    </td><td> 47</td><td>17.80</td></tr>
	<tr><td>por não precisa voltar na loja para buscar o produto</td><td> 28</td><td>10.61</td></tr>
	<tr><td>por nao precisar carregar os produtos               </td><td> 44</td><td>16.67</td></tr>
	<tr><td>por poder personalizar meu produto                  </td><td>  6</td><td> 2.27</td></tr>
	<tr><td>por receber orientacao do vendedor                  </td><td>  9</td><td> 3.41</td></tr>
</tbody>
</table>




![png](output_97_1.png)



```R
plot_multi('impedimento_compra_online', reorder = T, width = 10, title = 'O que te impede de fazer compras online?')
```


<table>
<caption>A data.frame: 11 × 3</caption>
<thead>
	<tr><th scope=col>value</th><th scope=col>Freq</th><th scope=col>pct</th></tr>
	<tr><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>a demora para receber o produto                          </td><td> 36</td><td> 6.47</td></tr>
	<tr><td>a falta de informações sobre o produto                   </td><td> 25</td><td> 4.50</td></tr>
	<tr><td>a falta de padronização dos tamanhos                     </td><td> 36</td><td> 6.47</td></tr>
	<tr><td>a incerteza se o produto ira servir                      </td><td>113</td><td>20.32</td></tr>
	<tr><td>a insegurança em relacao ao pagamento                    </td><td> 28</td><td> 5.04</td></tr>
	<tr><td>a inseguranca se o poduto realmente chegara              </td><td> 41</td><td> 7.37</td></tr>
	<tr><td>gostar da experiencia que tenho quando vou na loja física</td><td> 42</td><td> 7.55</td></tr>
	<tr><td>nada me impede de comprar online                         </td><td> 54</td><td> 9.71</td></tr>
	<tr><td>não poder experimentar o produto                         </td><td>110</td><td>19.78</td></tr>
	<tr><td>não poder ver e tocar o produto                          </td><td> 62</td><td>11.15</td></tr>
	<tr><td>outros                                                   </td><td>  9</td><td> 1.62</td></tr>
</tbody>
</table>




![png](output_98_1.png)



```R

```


```R

```


```R

```


```R

```


```R

```


```R

```


```R

```


```R

```


```R
ggplot(duda, aes(x = factor(gosta_compras), fill = factor(faixa_etaria))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('Faixa Etária') +
    labs(fill = 'Faixa Etária') +
    ggtitle('Gosta de fazer compras?')
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_107_1.png)



```R
with(duda, table(gosta_compras, faixa_etaria))
```


                 faixa_etaria
    gosta_compras 18-29 anos 30-45 anos 46-60 anos 61 anos ou mais até 17 anos
              não         10          4          6               0           0
              sim        119         63         47               6           9



```R
par(bg = 'white')
for (col1 in plot_cols){
    for (col2 in plot_cols){
        plot(as.numeric(duda[,col1]), as.numeric(duda[,col2]), xlab=as.character(col1), ylab=as.character(col2), col=rgb(0,0,0,0.1))
    }
}
```


    Error in eval(expr, envir, enclos): object 'plot_cols' not found
    Traceback:




```R
ggplot(duda, aes(faixa_etaria, genero)) +
    geom_point(alpha = 0.1)
```


```R

```
