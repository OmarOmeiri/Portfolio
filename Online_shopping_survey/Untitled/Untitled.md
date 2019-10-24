

```R
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape)
library(forcats)

options(repr.matrix.max.cols = 50)
opts <- options()
```


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
duda$faixa_etaria <- factor(duda$faixa_etaria, levels = c('até 17 anos', '18-29 anos', '30-45 anos', '46-60 anos', '61 anos ou mais'))

ggplot(duda, aes(x = factor(faixa_etaria), fill = factor(genero))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('Faixa Etária') +
    labs(fill = 'Gênero') +
    ggtitle('Distribuição da Faixa Etária por Gênero')
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_11_1.png)



```R
with(duda, table(genero, faixa_etaria))
```


               faixa_etaria
    genero      até 17 anos 18-29 anos 30-45 anos 46-60 anos 61 anos ou mais
      feminino            8         76         57         31               2
      masculino           1         53         10         22               4



```R
ggplot(duda, aes(x = factor(genero), fill = factor(genero))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('Faixa Etária') +
    labs(fill = 'Gênero') +
    ggtitle('Gênero')
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_13_1.png)



```R
with(duda, table(genero))
```


    genero
     feminino masculino 
          174        90 



```R
duda$regiao <- fct_rev(reorder(duda$regiao, duda$regiao,FUN=length))

ggplot(duda, aes(x = factor(regiao), fill = factor(regiao))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Região') +
    theme(axis.text.x = element_text(angle = 90))
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_15_1.png)



```R
with(duda, table(regiao))
```


    regiao
                    Curitiba                 exterior Fora do estado do Paraná 
                         237                        6                        5 
      Outra cidade do Paraná     Região Metropolitana 
                           8                        8 



```R
options(repr.plot.width = 10)

duda$ocupacao <- fct_rev(reorder(duda$ocupacao, duda$ocupacao,FUN=length))

ggplot(duda, aes(x = factor(ocupacao), fill = factor(ocupacao))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Ocupação') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_17_1.png)



```R
options(opts)
duda$renda_familiar <- factor(duda$renda_familiar, levels = c('até 2.000', '2.000 a 5.000', '5.000 a 10.000', '10.000 a 15.000', 'mais de 15.000'))

ggplot(duda, aes(x = factor(renda_familiar), fill = factor(renda_familiar))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Renda Familiar') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_18_1.png)



```R


ggplot(duda, aes(x = factor(gosta_compras), fill = factor(gosta_compras))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Gosta de fazer compras?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_19_1.png)



```R
duda$dificuldade_compra <- fct_rev(reorder(duda$dificuldade_compra, duda$dificuldade_compra,FUN=length))


options(repr.plot.width = 10)
ggplot(duda, aes(x = factor(dificuldade_compra), fill = factor(dificuldade_compra))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Dificuldade de Compra') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_20_1.png)



```R
tipo_loja <- na.exclude(subset(multi_df_long, startsWith(multi_df_long$key, 'tipo_loja')))

tipo_loja$value <- fct_rev(reorder(tipo_loja$value, tipo_loja$value,FUN=length))


options(repr.plot.width = 10)
ggplot(tipo_loja, aes(x = factor(value), fill = factor(value))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Que tipo de loja você mais gosta?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_21_1.png)



```R



fonte_info <- na.exclude(subset(multi_df_long, startsWith(multi_df_long$key, 'fonte')))

fonte_info$value <- fct_rev(reorder(fonte_info$value, fonte_info$value,FUN=length))


options(repr.plot.width = 10)
ggplot(fonte_info, aes(x = factor(value), fill = factor(value))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Como é a sua relação com as lojas onde gosta de comprar na internet?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_22_1.png)



```R



consideracao <- na.exclude(subset(multi_df_long, startsWith(multi_df_long$key, 'consid')))

consideracao$value <- fct_rev(reorder(consideracao$value, consideracao$value,FUN=length))


options(repr.plot.width = 10)
ggplot(consideracao, aes(x = factor(value), fill = factor(value))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('O que você leva em consideração na hora de escolher uma loja?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_23_1.png)



```R
relacao <- na.exclude(subset(multi_df_long, startsWith(multi_df_long$key, 'relac')))

relacao$value <- fct_rev(reorder(relacao$value, relacao$value,FUN=length))


options(repr.plot.width = 10)
ggplot(relacao, aes(x = factor(value), fill = factor(value))) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('O que você leva em consideração na hora de escolher uma loja?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_24_1.png)



```R
duda$meio_preferido <- fct_rev(reorder(duda$meio_preferido, duda$meio_preferido,FUN=length))

ggplot(duda, aes(x = meio_preferido, fill = meio_preferido)) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Qual o seu meio preferido para fazer compras?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_25_1.png)



```R
#papel_internet

duda$papel_internet <- fct_rev(reorder(duda$papel_internet, duda$papel_internet,FUN=length))

ggplot(duda, aes(x = papel_internet, fill = papel_internet)) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Qual o papel da internet na hora de fazer compras?') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_26_1.png)



```R
#faz_compra_online



duda$faz_compra_online <- fct_rev(reorder(duda$faz_compra_online, duda$faz_compra_online,FUN=length))

ggplot(duda, aes(x = faz_compra_online, fill = faz_compra_online)) + 
    geom_histogram(stat = 'count', position = 'identity') +
    ylab('Quantidade') + 
    xlab('') +
    labs(fill = '') +
    ggtitle('Você costuma fazer compras online??') +
    theme(axis.text.x = element_blank())
```

    Warning message:
    “Ignoring unknown parameters: binwidth, bins, pad”


![png](output_27_1.png)



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


![png](output_34_1.png)



```R
with(duda, table(gosta_compras, faixa_etaria))
```


                 faixa_etaria
    gosta_compras até 17 anos 18-29 anos 30-45 anos 46-60 anos 61 anos ou mais
              não           0         10          4          6               0
              sim           9        119         63         47               6



```R
par(bg = 'white')
for (col1 in plot_cols){
    for (col2 in plot_cols){
        plot(as.numeric(duda[,col1]), as.numeric(duda[,col2]), xlab=as.character(col1), ylab=as.character(col2), col=rgb(0,0,0,0.1))
    }
}
```


![png](output_36_0.png)



![png](output_36_1.png)



![png](output_36_2.png)



![png](output_36_3.png)



![png](output_36_4.png)



![png](output_36_5.png)



![png](output_36_6.png)



![png](output_36_7.png)



![png](output_36_8.png)



![png](output_36_9.png)



![png](output_36_10.png)



![png](output_36_11.png)



![png](output_36_12.png)



![png](output_36_13.png)



![png](output_36_14.png)



![png](output_36_15.png)



![png](output_36_16.png)



![png](output_36_17.png)



![png](output_36_18.png)



![png](output_36_19.png)



![png](output_36_20.png)



![png](output_36_21.png)



![png](output_36_22.png)



![png](output_36_23.png)



![png](output_36_24.png)



![png](output_36_25.png)



![png](output_36_26.png)



![png](output_36_27.png)



![png](output_36_28.png)



![png](output_36_29.png)



![png](output_36_30.png)



![png](output_36_31.png)



![png](output_36_32.png)



![png](output_36_33.png)



![png](output_36_34.png)



![png](output_36_35.png)



![png](output_36_36.png)



![png](output_36_37.png)



![png](output_36_38.png)



![png](output_36_39.png)



![png](output_36_40.png)



![png](output_36_41.png)



![png](output_36_42.png)



![png](output_36_43.png)



![png](output_36_44.png)



![png](output_36_45.png)



![png](output_36_46.png)



![png](output_36_47.png)



![png](output_36_48.png)



![png](output_36_49.png)



![png](output_36_50.png)



![png](output_36_51.png)



![png](output_36_52.png)



![png](output_36_53.png)



![png](output_36_54.png)



![png](output_36_55.png)



![png](output_36_56.png)



![png](output_36_57.png)



![png](output_36_58.png)



![png](output_36_59.png)



![png](output_36_60.png)



![png](output_36_61.png)



![png](output_36_62.png)



![png](output_36_63.png)



![png](output_36_64.png)



![png](output_36_65.png)



![png](output_36_66.png)



![png](output_36_67.png)



![png](output_36_68.png)



![png](output_36_69.png)



![png](output_36_70.png)



![png](output_36_71.png)



![png](output_36_72.png)



![png](output_36_73.png)



![png](output_36_74.png)



![png](output_36_75.png)



![png](output_36_76.png)



![png](output_36_77.png)



![png](output_36_78.png)



![png](output_36_79.png)



![png](output_36_80.png)



![png](output_36_81.png)



![png](output_36_82.png)



![png](output_36_83.png)



![png](output_36_84.png)



![png](output_36_85.png)



![png](output_36_86.png)



![png](output_36_87.png)



![png](output_36_88.png)



![png](output_36_89.png)



![png](output_36_90.png)



![png](output_36_91.png)



![png](output_36_92.png)



![png](output_36_93.png)



![png](output_36_94.png)



![png](output_36_95.png)



![png](output_36_96.png)



![png](output_36_97.png)



![png](output_36_98.png)



![png](output_36_99.png)



![png](output_36_100.png)



![png](output_36_101.png)



![png](output_36_102.png)



![png](output_36_103.png)



![png](output_36_104.png)



![png](output_36_105.png)



![png](output_36_106.png)



![png](output_36_107.png)



![png](output_36_108.png)



![png](output_36_109.png)



![png](output_36_110.png)



![png](output_36_111.png)



![png](output_36_112.png)



![png](output_36_113.png)



![png](output_36_114.png)



![png](output_36_115.png)



![png](output_36_116.png)



![png](output_36_117.png)



![png](output_36_118.png)



![png](output_36_119.png)



![png](output_36_120.png)



![png](output_36_121.png)



![png](output_36_122.png)



![png](output_36_123.png)



![png](output_36_124.png)



![png](output_36_125.png)



![png](output_36_126.png)



![png](output_36_127.png)



![png](output_36_128.png)



![png](output_36_129.png)



![png](output_36_130.png)



![png](output_36_131.png)



![png](output_36_132.png)



![png](output_36_133.png)



![png](output_36_134.png)



![png](output_36_135.png)



![png](output_36_136.png)



![png](output_36_137.png)



![png](output_36_138.png)



![png](output_36_139.png)



![png](output_36_140.png)



![png](output_36_141.png)



![png](output_36_142.png)



![png](output_36_143.png)



![png](output_36_144.png)



![png](output_36_145.png)



![png](output_36_146.png)



![png](output_36_147.png)



![png](output_36_148.png)



![png](output_36_149.png)



![png](output_36_150.png)



![png](output_36_151.png)



![png](output_36_152.png)



![png](output_36_153.png)



![png](output_36_154.png)



![png](output_36_155.png)



![png](output_36_156.png)



![png](output_36_157.png)



![png](output_36_158.png)



![png](output_36_159.png)



![png](output_36_160.png)



![png](output_36_161.png)



![png](output_36_162.png)



![png](output_36_163.png)



![png](output_36_164.png)



![png](output_36_165.png)



![png](output_36_166.png)



![png](output_36_167.png)



![png](output_36_168.png)



![png](output_36_169.png)



![png](output_36_170.png)



![png](output_36_171.png)



![png](output_36_172.png)



![png](output_36_173.png)



![png](output_36_174.png)



![png](output_36_175.png)



![png](output_36_176.png)



![png](output_36_177.png)



![png](output_36_178.png)



![png](output_36_179.png)



![png](output_36_180.png)



![png](output_36_181.png)



![png](output_36_182.png)



![png](output_36_183.png)



![png](output_36_184.png)



![png](output_36_185.png)



![png](output_36_186.png)



![png](output_36_187.png)



![png](output_36_188.png)



![png](output_36_189.png)



![png](output_36_190.png)



![png](output_36_191.png)



![png](output_36_192.png)



![png](output_36_193.png)



![png](output_36_194.png)



![png](output_36_195.png)



![png](output_36_196.png)



![png](output_36_197.png)



![png](output_36_198.png)



![png](output_36_199.png)



![png](output_36_200.png)



![png](output_36_201.png)



![png](output_36_202.png)



![png](output_36_203.png)



![png](output_36_204.png)



![png](output_36_205.png)



![png](output_36_206.png)



![png](output_36_207.png)



![png](output_36_208.png)



![png](output_36_209.png)



![png](output_36_210.png)



![png](output_36_211.png)



![png](output_36_212.png)



![png](output_36_213.png)



![png](output_36_214.png)



![png](output_36_215.png)



![png](output_36_216.png)



![png](output_36_217.png)



![png](output_36_218.png)



![png](output_36_219.png)



![png](output_36_220.png)



![png](output_36_221.png)



![png](output_36_222.png)



![png](output_36_223.png)



![png](output_36_224.png)



![png](output_36_225.png)



![png](output_36_226.png)



![png](output_36_227.png)



![png](output_36_228.png)



![png](output_36_229.png)



![png](output_36_230.png)



![png](output_36_231.png)



![png](output_36_232.png)



![png](output_36_233.png)



![png](output_36_234.png)



![png](output_36_235.png)



![png](output_36_236.png)



![png](output_36_237.png)



![png](output_36_238.png)



![png](output_36_239.png)



![png](output_36_240.png)



![png](output_36_241.png)



![png](output_36_242.png)



![png](output_36_243.png)



![png](output_36_244.png)



![png](output_36_245.png)



![png](output_36_246.png)



![png](output_36_247.png)



![png](output_36_248.png)



![png](output_36_249.png)



![png](output_36_250.png)



![png](output_36_251.png)



![png](output_36_252.png)



![png](output_36_253.png)



![png](output_36_254.png)



![png](output_36_255.png)



![png](output_36_256.png)



![png](output_36_257.png)



![png](output_36_258.png)



![png](output_36_259.png)



![png](output_36_260.png)



![png](output_36_261.png)



![png](output_36_262.png)



![png](output_36_263.png)



![png](output_36_264.png)



![png](output_36_265.png)



![png](output_36_266.png)



![png](output_36_267.png)



![png](output_36_268.png)



![png](output_36_269.png)



![png](output_36_270.png)



![png](output_36_271.png)



![png](output_36_272.png)



![png](output_36_273.png)



![png](output_36_274.png)



![png](output_36_275.png)



![png](output_36_276.png)



![png](output_36_277.png)



![png](output_36_278.png)



![png](output_36_279.png)



![png](output_36_280.png)



![png](output_36_281.png)



![png](output_36_282.png)



![png](output_36_283.png)



![png](output_36_284.png)



![png](output_36_285.png)



![png](output_36_286.png)



![png](output_36_287.png)



![png](output_36_288.png)



![png](output_36_289.png)



![png](output_36_290.png)



![png](output_36_291.png)



![png](output_36_292.png)



![png](output_36_293.png)



![png](output_36_294.png)



![png](output_36_295.png)



![png](output_36_296.png)



![png](output_36_297.png)



![png](output_36_298.png)



![png](output_36_299.png)



![png](output_36_300.png)



![png](output_36_301.png)



![png](output_36_302.png)



![png](output_36_303.png)



![png](output_36_304.png)



![png](output_36_305.png)



![png](output_36_306.png)



![png](output_36_307.png)



![png](output_36_308.png)



![png](output_36_309.png)



![png](output_36_310.png)



![png](output_36_311.png)



![png](output_36_312.png)



![png](output_36_313.png)



![png](output_36_314.png)



![png](output_36_315.png)



![png](output_36_316.png)



![png](output_36_317.png)



![png](output_36_318.png)



![png](output_36_319.png)



![png](output_36_320.png)



![png](output_36_321.png)



![png](output_36_322.png)



![png](output_36_323.png)



![png](output_36_324.png)



![png](output_36_325.png)



![png](output_36_326.png)



![png](output_36_327.png)



![png](output_36_328.png)



![png](output_36_329.png)



![png](output_36_330.png)



![png](output_36_331.png)



![png](output_36_332.png)



![png](output_36_333.png)



![png](output_36_334.png)



![png](output_36_335.png)



![png](output_36_336.png)



![png](output_36_337.png)



![png](output_36_338.png)



![png](output_36_339.png)



![png](output_36_340.png)



![png](output_36_341.png)



![png](output_36_342.png)



![png](output_36_343.png)



![png](output_36_344.png)



![png](output_36_345.png)



![png](output_36_346.png)



![png](output_36_347.png)



![png](output_36_348.png)



![png](output_36_349.png)



![png](output_36_350.png)



![png](output_36_351.png)



![png](output_36_352.png)



![png](output_36_353.png)



![png](output_36_354.png)



![png](output_36_355.png)



![png](output_36_356.png)



![png](output_36_357.png)



![png](output_36_358.png)



![png](output_36_359.png)



![png](output_36_360.png)



![png](output_36_361.png)



![png](output_36_362.png)



![png](output_36_363.png)



![png](output_36_364.png)



![png](output_36_365.png)



![png](output_36_366.png)



![png](output_36_367.png)



![png](output_36_368.png)



![png](output_36_369.png)



![png](output_36_370.png)



![png](output_36_371.png)



![png](output_36_372.png)



![png](output_36_373.png)



![png](output_36_374.png)



![png](output_36_375.png)



![png](output_36_376.png)



![png](output_36_377.png)



![png](output_36_378.png)



![png](output_36_379.png)



![png](output_36_380.png)



![png](output_36_381.png)



![png](output_36_382.png)



![png](output_36_383.png)



![png](output_36_384.png)



![png](output_36_385.png)



![png](output_36_386.png)



![png](output_36_387.png)



![png](output_36_388.png)



![png](output_36_389.png)



![png](output_36_390.png)



![png](output_36_391.png)



![png](output_36_392.png)



![png](output_36_393.png)



![png](output_36_394.png)



![png](output_36_395.png)



![png](output_36_396.png)



![png](output_36_397.png)



![png](output_36_398.png)



![png](output_36_399.png)



![png](output_36_400.png)



![png](output_36_401.png)



![png](output_36_402.png)



![png](output_36_403.png)



![png](output_36_404.png)



![png](output_36_405.png)



![png](output_36_406.png)



![png](output_36_407.png)



![png](output_36_408.png)



![png](output_36_409.png)



![png](output_36_410.png)



![png](output_36_411.png)



![png](output_36_412.png)



![png](output_36_413.png)



![png](output_36_414.png)



![png](output_36_415.png)



![png](output_36_416.png)



![png](output_36_417.png)



![png](output_36_418.png)



![png](output_36_419.png)



![png](output_36_420.png)



![png](output_36_421.png)



![png](output_36_422.png)



![png](output_36_423.png)



![png](output_36_424.png)



![png](output_36_425.png)



![png](output_36_426.png)



![png](output_36_427.png)



![png](output_36_428.png)



![png](output_36_429.png)



![png](output_36_430.png)



![png](output_36_431.png)



![png](output_36_432.png)



![png](output_36_433.png)



![png](output_36_434.png)



![png](output_36_435.png)



![png](output_36_436.png)



![png](output_36_437.png)



![png](output_36_438.png)



![png](output_36_439.png)



![png](output_36_440.png)



![png](output_36_441.png)



![png](output_36_442.png)



![png](output_36_443.png)



![png](output_36_444.png)



![png](output_36_445.png)



![png](output_36_446.png)



![png](output_36_447.png)



![png](output_36_448.png)



![png](output_36_449.png)



![png](output_36_450.png)



![png](output_36_451.png)



![png](output_36_452.png)



![png](output_36_453.png)



![png](output_36_454.png)



![png](output_36_455.png)



![png](output_36_456.png)



![png](output_36_457.png)



![png](output_36_458.png)



![png](output_36_459.png)



![png](output_36_460.png)



![png](output_36_461.png)



![png](output_36_462.png)



![png](output_36_463.png)



![png](output_36_464.png)



![png](output_36_465.png)



![png](output_36_466.png)



![png](output_36_467.png)



![png](output_36_468.png)



![png](output_36_469.png)



![png](output_36_470.png)



![png](output_36_471.png)



![png](output_36_472.png)



![png](output_36_473.png)



![png](output_36_474.png)



![png](output_36_475.png)



![png](output_36_476.png)



![png](output_36_477.png)



![png](output_36_478.png)



![png](output_36_479.png)



![png](output_36_480.png)



![png](output_36_481.png)



![png](output_36_482.png)



![png](output_36_483.png)



![png](output_36_484.png)



![png](output_36_485.png)



![png](output_36_486.png)



![png](output_36_487.png)



![png](output_36_488.png)



![png](output_36_489.png)



![png](output_36_490.png)



![png](output_36_491.png)



![png](output_36_492.png)



![png](output_36_493.png)



![png](output_36_494.png)



![png](output_36_495.png)



![png](output_36_496.png)



![png](output_36_497.png)



![png](output_36_498.png)



![png](output_36_499.png)



![png](output_36_500.png)



![png](output_36_501.png)



![png](output_36_502.png)



![png](output_36_503.png)



![png](output_36_504.png)



![png](output_36_505.png)



![png](output_36_506.png)



![png](output_36_507.png)



![png](output_36_508.png)



![png](output_36_509.png)



![png](output_36_510.png)



![png](output_36_511.png)



![png](output_36_512.png)



![png](output_36_513.png)



![png](output_36_514.png)



![png](output_36_515.png)



![png](output_36_516.png)



![png](output_36_517.png)



![png](output_36_518.png)



![png](output_36_519.png)



![png](output_36_520.png)



![png](output_36_521.png)



![png](output_36_522.png)



![png](output_36_523.png)



![png](output_36_524.png)



![png](output_36_525.png)



![png](output_36_526.png)



![png](output_36_527.png)



![png](output_36_528.png)



```R
ggplot(duda, aes(faixa_etaria, genero)) +
    geom_point(alpha = 0.1)
```


![png](output_37_0.png)



```R

```
